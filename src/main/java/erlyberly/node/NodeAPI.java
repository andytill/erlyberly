/**
 * erlyberly, erlang trace debugger
 * Copyright (C) 2016 Andy Till
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package erlyberly.node;

import static erlyberly.node.OtpUtil.OK_ATOM;
import static erlyberly.node.OtpUtil.atom;
import static erlyberly.node.OtpUtil.isTupleTagged;
import static erlyberly.node.OtpUtil.list;
import static erlyberly.node.OtpUtil.tuple;

import java.io.IOException;
import java.io.InputStream;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicLong;

import com.ericsson.otp.erlang.OtpAuthException;
import com.ericsson.otp.erlang.OtpConn;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangFun;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpPeer;
import com.ericsson.otp.erlang.OtpSelfNode;

import erlyberly.ModFunc;
import erlyberly.PrefBind;
import erlyberly.ProcInfo;
import erlyberly.SeqTraceLog;
import erlyberly.TraceLog;
import javafx.application.Platform;
import javafx.beans.Observable;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

public class NodeAPI {
    private static final OtpErlangAtom ERLYBERLY_TRACE_LOG = atom("erlyberly_trace_log");

    private static final OtpErlangAtom ERLYBERLY_XREF_STARTED_ATOM = atom("erlyberly_xref_started");

    private static final OtpErlangAtom ERLYBERLY_ERROR_REPORT_ATOM = atom("erlyberly_error_report");

    private static final OtpErlangAtom ERLYBERLY_MODULE_RELOADED_ATOM = atom("erlyberly_module_loaded");

    private static final OtpErlangAtom ERLYBERLY_ATOM = new OtpErlangAtom("erlyberly");

    private static final OtpErlangAtom BET_SERVICES_MSG_ATOM = new OtpErlangAtom("add_locator");

    public interface RpcCallback<T> {
        void callback(T result);
    }

    private static final OtpErlangAtom MODULE_ATOM = new OtpErlangAtom("module");

    private static final String ERLYBERLY_BEAM_PATH = "/erlyberly/beam/erlyberly.beam";

    private static final int BEAM_SIZE_LIMIT = 1024 * 50;

    private final TraceManager traceManager;

    private final SimpleBooleanProperty connectedProperty;

    private final SimpleBooleanProperty xrefStartedProperty;

    private final SimpleStringProperty summary;

    private OtpConn connection;

    private OtpSelfNode self;

    private String remoteNodeName;

    private String cookie;

    private volatile Thread checkAliveThread;

    private final SimpleObjectProperty<AppProcs> appProcs;

    private OtpMbox mbox;

    private volatile boolean connected = false;

    private final ObservableList<OtpErlangObject> crashReports = FXCollections.observableArrayList();

    private boolean manuallyDisconnected = false;

    private RpcCallback<OtpErlangTuple> moduleLoadedCallback;

    /**
     * Called when a trace log is received.
     * <br/>
     * Should only accessed from the FX thread.
     */
    private RpcCallback<TraceLog> traceLogCallback;

    public NodeAPI() {
        traceManager = new TraceManager();

        connectedProperty = new SimpleBooleanProperty();
        connectedProperty.addListener(new ChangeListener<Boolean>() {
            @Override
            public void changed(ObservableValue<? extends Boolean> obv, Boolean o, Boolean n) {
                connected = n;
            }});
        summary = new SimpleStringProperty("erlyberly not connected");

        appProcs = new SimpleObjectProperty<AppProcs>(new AppProcs(0, LocalDateTime.now()));

        connectedProperty.addListener(this::summaryUpdater);

        xrefStartedProperty = new SimpleBooleanProperty(false);
    }

    public NodeAPI connectionInfo(String remoteNodeName, String cookie) {
        this.remoteNodeName = remoteNodeName;
        this.cookie = cookie;

        return this;
    }

    public ObservableList<OtpErlangObject> getCrashReports() {
        return crashReports;
    }

    public SimpleObjectProperty<AppProcs> appProcsProperty() {
        return appProcs;
    }

    public synchronized void manualConnect() throws IOException, OtpErlangException, OtpAuthException {
        // TODO: here, we've cleared (set to false) being Manually/intentionally disconnected.
        manuallyDisconnected = false;
        connect();
    }

    public synchronized void connect() throws IOException, OtpErlangException, OtpAuthException {
        assert !Platform.isFxApplicationThread() : "cannot run this method from the FX thread";

        // clear previous connections and threads if any, before we reconnect
        // TODO: investigate whether we need this ...
        disconnect();

        self = new OtpSelfNode("erlyberly-" + System.currentTimeMillis());
        if(!cookie.isEmpty()) {
            self.setCookie(cookie);
        }

        // if the node name does not contain a host then assume it is on the
        // same machine
        if(!remoteNodeName.contains("@")) {
            String[] split = self.toString().split("\\@");

            remoteNodeName += "@" + split[1];
        }

        connection = self.connect(new OtpPeer(remoteNodeName));

        mbox = self.createMbox();

        loadRemoteErlyberly();

        addErrorLoggerHandler();

        // start dbg so we can listen for module loads
        ensureDbgStarted();

        loadModulesOnPath(PrefBind.getOrDefault("loadModulesRegex", "").toString());

        Platform.runLater(() -> { connectedProperty.set(true); });

        checkAliveThread = new CheckAliveThread();
        checkAliveThread.start();
    }

    public void manuallyDisconnect() throws IOException, OtpErlangException{
        // TODO: have a look at this: ( How can we properly "Close", or is the below acceptable? )
        // com.ericsson.otp.erlang.OtpErlangExit: 'Remote has closed connection'
        // at com.ericsson.otp.erlang.AbstractConnection.run(AbstractConnection.java:733)
        manuallyDisconnected = true;
        stopAllTraces();
        removeErrorLoggerHandler();
        unloadRemoteErlyberly();
        mbox.close();
        Platform.runLater(() -> { connectedProperty.set(false); });
    }

    public void disconnect() {
        try {
            if (connection != null)
                connection.close();
        }
        catch(Exception e) {
            System.out.println(e);
        }
        try {
            if (self != null)
                self.close();
        }
        catch(Exception e) {
            System.out.println(e);
        }
        connection = null;
        self = null;
    }

    private synchronized void ensureDbgStarted() throws IOException, OtpErlangException {
        sendRPC(
            "erlyberly", "ensure_dbg_started",
            list(tuple(atom(self.node()), mbox.self()))
        );
        // flush the return value
        receiveRPC();
    }

    private void loadModulesOnPath(String regex) throws IOException, OtpErlangException {
        if(regex == null || "".equals(regex))
            return;
        sendRPC(
            "erlyberly", "load_modules_on_path",
            list(new OtpErlangString(regex))
        );
        // flush the return value
        receiveRPC();
    }

    private synchronized void addErrorLoggerHandler() throws IOException, OtpErlangException {
        OtpErlangList args = OtpUtil.list(mbox.self());
        sendRPC(
            "error_logger", "add_report_handler",
            OtpUtil.list(ERLYBERLY_ATOM, args)
        );

        // flush the return value
        receiveRPC();
    }

    private synchronized void removeErrorLoggerHandler() throws IOException, OtpErlangException {
        OtpErlangList args = OtpUtil.list(mbox.self());
        sendRPC(
            "error_logger", "delete_report_handler",
            OtpUtil.list(ERLYBERLY_ATOM, args)
        );

        // flush the return value
        receiveRPC();
    }

    private static final AtomicLong CHECK_ALIVE_THREAD_COUNTER = new AtomicLong();

    class CheckAliveThread extends Thread {

        public CheckAliveThread() {
            setDaemon(true);
            setName("Erlyberly Check Alive "+ CHECK_ALIVE_THREAD_COUNTER.incrementAndGet());
        }

        @Override
        public void run() {
            while(true) {
                if (manuallyDisconnected){
                    this.interrupt();
                    break;
                }else{
                    ensureAlive();
                    mySleep(150);
                }
            }
        }
    }

    private void loadRemoteErlyberly() throws IOException, OtpErlangException {

        OtpErlangBinary otpErlangBinary = new OtpErlangBinary(loadBeamFile());

        sendRPC("code", "load_binary",
                list(
                        atom("erlyberly"),
                        new OtpErlangString(ERLYBERLY_BEAM_PATH),
                        otpErlangBinary));

        OtpErlangObject result = receiveRPC();

        if(result instanceof OtpErlangTuple) {
            OtpErlangObject e0 = ((OtpErlangTuple) result).elementAt(0);

            if(!MODULE_ATOM.equals(e0)) {
                throw new RuntimeException("error loading the erlyberly module, result was " + result);
            }
        }
        else {
            throw new RuntimeException("error loading the erlyberly module, result was " + result);
        }
    }

    private void unloadRemoteErlyberly() throws IOException, OtpErlangException {
        sendRPC("code", "purge", list(atom("erlyberly")));
        receiveRPC();
        sendRPC("code", "delete", list(atom("erlyberly")));
        receiveRPC();
        sendRPC("code", "soft_purge", list(atom("erlyberly")));
        receiveRPC();
    }

    private OtpErlangObject receiveRPC() throws IOException, OtpErlangException {
        int timeout = 5000;
        return receiveRPC(timeout);
    }

    private OtpErlangObject receiveRPC(int timeout) throws OtpErlangExit,
            OtpErlangDecodeException, IOException, OtpErlangException {
        OtpErlangTuple receive = OtpUtil.receiveRPC(mbox, timeout);

        if(receive == null) {
            return null;
        }
        else if(isTupleTagged(ERLYBERLY_TRACE_LOG, receive)) {
            traceLogNotification(receive);
            return receiveRPC(timeout);
        }
        else if(isTupleTagged(ERLYBERLY_ERROR_REPORT_ATOM, receive)) {
            Platform.runLater(() -> { crashReports.add(receive.elementAt(1)); });
            return receiveRPC(timeout);
        }
        else if(isTupleTagged(ERLYBERLY_MODULE_RELOADED_ATOM, receive)) {
            Platform.runLater(() -> {
                if(moduleLoadedCallback != null)
                    moduleLoadedCallback.callback((OtpErlangTuple) receive.elementAt(2));
            });
            return receiveRPC(timeout);
        }
        else if(!isTupleTagged(atom("rex"), receive)) {
            throw new RuntimeException("Expected tuple tagged with atom rex but got " + receive);
        }
        OtpErlangObject result = receive.elementAt(1);

        // hack to support certain projects, don't ask...
        if(isTupleTagged(BET_SERVICES_MSG_ATOM, result)) {
            result = receiveRPC(timeout);
        }
        else if(isTupleTagged(ERLYBERLY_XREF_STARTED_ATOM, result)) {
            Platform.runLater(() -> { xrefStartedProperty.set(true); });
            return receiveRPC(timeout);
        }

        return result;
    }

    private void traceLogNotification(OtpErlangTuple receive) {
        Platform.runLater(() -> {
            OtpErlangTuple traceLog = (OtpErlangTuple) ((OtpErlangTuple) receive).elementAt(1);
            ArrayList<TraceLog> collatedTraces = traceManager.collateTraceSingle(traceLog);
            if(traceLogCallback != null) {
                for (TraceLog log : collatedTraces) {
                    traceLogCallback.callback(log);
                }
            }
        });
    }

    private static byte[] loadBeamFile() throws IOException {
        InputStream resourceAsStream = OtpUtil.class.getResourceAsStream(ERLYBERLY_BEAM_PATH);

        byte[] b = new byte[BEAM_SIZE_LIMIT];
        int total = 0;
        int read = 0;

        do {
            total += read;
            read = resourceAsStream.read(b, total, BEAM_SIZE_LIMIT - total);
        } while (read != -1);

        if(total >= BEAM_SIZE_LIMIT) {
            throw new RuntimeException("erlyberly.beam file is too big");
        }

        return Arrays.copyOf(b, total);
    }

    public synchronized void retrieveProcessInfo(ArrayList<ProcInfo> processes) throws Exception {
        assert !Platform.isFxApplicationThread() : "cannot run this method from the FX thread";

        if(connection == null || !connected)
            return;

        OtpErlangObject receiveRPC = null;

        try {
            sendRPC("erlyberly", "process_info", new OtpErlangList());
            receiveRPC = receiveRPC();
            OtpErlangList received = (OtpErlangList) receiveRPC;

            for (OtpErlangObject recv : received) {
                if(recv instanceof OtpErlangList) {
                    OtpErlangList pinfo = (OtpErlangList) recv;
                    HashMap<Object, Object> propsToMap = OtpUtil.propsToMap(pinfo);
                    processes.add(ProcInfo.toProcessInfo(propsToMap));
                }
                }
            Platform.runLater(() -> { appProcs.set(new AppProcs(processes.size(), LocalDateTime.now())); });
        }
        catch (ClassCastException e) {
            throw new RuntimeException("unexpected result: " + receiveRPC, e);
        }
    }

    private synchronized boolean ensureAlive() {
        try {
            receiveRPC(0);
            if(connection != null && connection.isAlive())
                return true;
        }
        catch(OtpErlangExit oee) {
            // an exit is what we're checking for so no need to log it
        }
        catch (OtpErlangException | IOException e1) {
            e1.printStackTrace();
        }

        Platform.runLater(() -> { connectedProperty.set(false); });

        while(true) {
            try {
                if(!this.manuallyDisconnected){
                    connect();
                    break;
                }
            }
            catch(Exception e) {
                int millis = 50;
                mySleep(millis);

            }
        }
        return true;
    }

    private void mySleep(int millis) {
        try {
            Thread.sleep(millis);
        } catch (InterruptedException e1) {
            e1.printStackTrace();
        }
    }

    public synchronized OtpErlangList requestFunctions() throws Exception {
        assert !Platform.isFxApplicationThread() : "cannot run this method from the FX thread";

        sendRPC("erlyberly", "module_functions", new OtpErlangList());
        return (OtpErlangList) receiveRPC();
    }

    public synchronized void startTrace(ModFunc mf) throws Exception {
        assert mf.getFuncName() != null : "function name cannot be null";

        sendRPC("erlyberly", "start_trace", toTraceTuple(mf));

        OtpErlangObject result = receiveRPC();

        if(isTupleTagged(atom("error"), result)) {
            System.out.println(result);


            // TODO notify caller of failure!
            return;
        }
    }

    public synchronized void stopTrace(ModFunc mf) throws Exception {
        assert mf.getFuncName() != null : "function name cannot be null";
        sendRPC("erlyberly", "stop_trace",
            list(
                OtpUtil.atom(mf.getModuleName()),
                OtpUtil.atom(mf.getFuncName()),
                new OtpErlangInt(mf.getArity()),
                new OtpErlangAtom(mf.isExported())
            ));
        receiveRPC();
    }

    public synchronized void stopAllTraces() throws IOException, OtpErlangException {
        sendRPC("erlyberly", "stop_traces", list());
        receiveRPC();
    }

    private OtpErlangList toTraceTuple(ModFunc mf) {
        return list(
            tuple(OtpUtil.atom(self.node()), mbox.self()),
            atom(mf.getModuleName()),
            atom(mf.getFuncName()),
            mf.getArity(),
            mf.isExported()
        );
    }

    public SimpleBooleanProperty connectedProperty() {
        return connectedProperty;
    }

    public SimpleBooleanProperty xrefStartedProperty() {
        return xrefStartedProperty;
    }

    private void sendRPC(String module, String function, OtpErlangList args) throws IOException {
        OtpUtil.sendRPC(connection, mbox, atom(module), atom(function), args);
    }

    public synchronized ArrayList<TraceLog> collectTraceLogs() throws Exception {
        sendRPC("erlyberly", "collect_trace_logs", new OtpErlangList());
        OtpErlangObject prcResult = receiveRPC();
        if(!isTupleTagged(OK_ATOM, prcResult)) {
            if(prcResult != null) {
                System.out.println(prcResult);
            }
            return new ArrayList<TraceLog>();
        }
        OtpErlangList traceLogs = (OtpErlangList) ((OtpErlangTuple) prcResult).elementAt(1);
        return traceManager.collateTraces(traceLogs);
    }

    public synchronized ArrayList<SeqTraceLog> collectSeqTraceLogs() throws Exception {
        sendRPC("erlyberly", "collect_seq_trace_logs", new OtpErlangList());

        OtpErlangObject prcResult = receiveRPC();

        if(!isTupleTagged(OK_ATOM, prcResult)) {
            return new ArrayList<SeqTraceLog>();
        }

        ArrayList<SeqTraceLog> seqLogs = new ArrayList<SeqTraceLog>();

        try {
            OtpErlangList traceLogs = (OtpErlangList) ((OtpErlangTuple) prcResult)
                    .elementAt(1);
            for (OtpErlangObject otpErlangObject : traceLogs) {
                seqLogs.add(SeqTraceLog.build(OtpUtil
                        .propsToMap((OtpErlangList) otpErlangObject)));
            }
        }
        catch (ClassCastException e) {
            System.out.println("did not understand result from collect_seq_trace_logs " + prcResult);
            e.printStackTrace();
        }
        return seqLogs;
    }

    public ObservableValue<? extends String> summaryProperty() {
        return summary;
    }

    private void summaryUpdater(Observable o, Boolean wasConnected, Boolean isConnected) {
        String summaryText = "erlyberly";

        if(!wasConnected && isConnected)
            summaryText = self.node() + " connected to " + this.remoteNodeName;
        else if(wasConnected && !isConnected)
            summaryText = "erlyberly, connection lost.  reconnecting...";

        summary.set(summaryText);
    }

    public synchronized void seqTrace(ModFunc mf) throws IOException, OtpErlangException {
        sendRPC("erlyberly", "seq_trace",
            list(
                tuple(OtpUtil.atom(self.node()), mbox.self()),
                atom(mf.getModuleName()),
                atom(mf.getFuncName()),
                mf.getArity(),
                new OtpErlangAtom(mf.isExported())
            ));

        OtpErlangObject result = receiveRPC();

        System.out.println(result);
    }

    public synchronized OtpErlangObject getProcessState(String pidString) throws IOException, OtpErlangException {
        sendRPC("erlyberly", "get_process_state", list(pidString));

        OtpErlangObject result = receiveRPC();

        if(isTupleTagged(OK_ATOM, result)) {
            return ((OtpErlangTuple)result).elementAt(1);
        }
        return null;
    }

    public synchronized HashMap<Object, Object> erlangMemory() throws IOException, OtpErlangException {
        sendRPC("erlang", "memory", list());

        OtpErlangList result = (OtpErlangList) receiveRPC();

        return OtpUtil.propsToMap(result);
    }

    public boolean isConnected() {
        return connected;
    }

    public boolean manuallyDisconnected(){
        return manuallyDisconnected;
    }

    public synchronized OtpErlangObject callGraph(OtpErlangAtom module, OtpErlangAtom function, OtpErlangLong arity) throws IOException, OtpErlangException {
        sendRPC("erlyberly", "xref_analysis", list(module, function, arity));

        OtpErlangObject result = (OtpErlangObject) receiveRPC();

        return result;
    }

    /**
     * Start xref but
     */
    public synchronized void asyncEnsureXRefStarted() throws IOException {
        sendRPC("erlyberly", "ensure_xref_started", list());
    }

    public synchronized String moduleFunctionSourceCode(String module, String function, Integer arity) throws IOException, OtpErlangException {
        OtpErlangInt otpArity = new OtpErlangInt(arity);
        sendRPC("erlyberly", "get_source_code", list( tuple(atom(module), atom(function), otpArity) ));
        OtpErlangObject result = receiveRPC();
        return returnCode(result, "Failed to get source code for " + module + ":" + function + "/" + arity.toString() + ".");
    }

    public synchronized String moduleFunctionSourceCode(String module) throws IOException, OtpErlangException {
        sendRPC("erlyberly", "get_source_code", list( atom(module) ));
        OtpErlangObject result = receiveRPC();
        return returnCode(result, "Failed to get source code for " + module + ".");
    }

    public synchronized String moduleFunctionAbstCode(String module) throws IOException, OtpErlangException {
        sendRPC("erlyberly", "get_abstract_code", list(atom(module)));
        OtpErlangObject result = receiveRPC();
        return returnCode(result, "Failed to get abstract code for " + module + ".");
    }
    public synchronized String moduleFunctionAbstCode(String module, String function, Integer arity) throws IOException, OtpErlangException {
        OtpErlangInt otpArity = new OtpErlangInt(arity);
        sendRPC("erlyberly", "get_abstract_code", list( tuple(atom(module), atom(function), otpArity) ));
        OtpErlangObject result = receiveRPC();
        return returnCode(result, "Failed to get abstract code for " + module + ".");
    }

    public String returnCode(OtpErlangObject result, String errorResponse){
        if(isTupleTagged(OK_ATOM, result)) {
            OtpErlangBinary bin = (OtpErlangBinary) ((OtpErlangTuple)result).elementAt(1);
            String ss = new String(bin.binaryValue());
            return ss;
        } else {
            OtpErlangBinary bin = (OtpErlangBinary) ((OtpErlangTuple)result).elementAt(1);
            String err = new String(bin.binaryValue());
            System.out.println(err);
            return errorResponse;
        }
    }

    public synchronized OtpErlangList dictToPropslist(OtpErlangObject dict) throws IOException, OtpErlangException {
        sendRPC("dict", "to_list", list(dict));
        return (OtpErlangList) receiveRPC(5000);
    }

    /**
     * Set the callback that is invoked when erlyberly receives a message that a
     * module has been loaded, or reloaded by the VM. The callback argument is in
     * the format {module(), ExportedFuncs, UnexportedFuncs}. A function is the
     * format {atom(), integer()}.
     */
    public void setModuleLoadedCallback(RpcCallback<OtpErlangTuple> aModuleLoadedCallback) {
        moduleLoadedCallback = aModuleLoadedCallback;
    }

    public synchronized String decompileFun(OtpErlangFun fun) throws IOException, OtpErlangException {
        sendRPC("erlyberly", "saleyn_fun_src", list(fun));
        OtpErlangObject received = receiveRPC(5000);
        if(received instanceof OtpErlangString) {
            OtpErlangString otpString = (OtpErlangString) received;
            return otpString.stringValue();
        }
        else {
            throw new OtpErlangException(Objects.toString(received));
        }
    }

    public RpcCallback<TraceLog> getTraceLogCallback() {
        return traceLogCallback;
    }

    public void setTraceLogCallback(RpcCallback<TraceLog> traceLogCallback) {
        this.traceLogCallback = traceLogCallback;
    }
}
