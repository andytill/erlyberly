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

import static erlyberly.node.OtpUtil.atom;
import static erlyberly.node.OtpUtil.isTupleTagged;
import static erlyberly.node.OtpUtil.list;
import static erlyberly.node.OtpUtil.tuple;

import java.io.IOException;
import java.io.InputStream;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Random;
import java.util.concurrent.atomic.AtomicLong;

import com.ericsson.otp.erlang.OtpAuthException;
import com.ericsson.otp.erlang.OtpConn;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangFun;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpPeer;
import com.ericsson.otp.erlang.OtpSelfNode;

import erlyberly.ErlyBerly;
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

    private static final OtpErlangAtom TRY_LOAD_MODULE_ATOM = atom("try_load_module");

    private static final OtpErlangAtom ERROR_ATOM = atom("error");

    private static final OtpErlangAtom ERLYBERLY_TRACE_OVERLOAD_ATOM = atom("erlyberly_trace_overload");

    private static final String ERLYBERLY = "erlyberly";

    private static final String CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD = "cannot run this method from the FX thread";

    private static final OtpErlangAtom ERLYBERLY_TRACE_LOG = atom("erlyberly_trace_log");

    private static final OtpErlangAtom ERLYBERLY_ERROR_REPORT_ATOM = atom("erlyberly_error_report");

    private static final OtpErlangAtom ERLYBERLY_MODULE_RELOADED_ATOM = atom("erlyberly_module_loaded");

    private static final OtpErlangAtom ERLYBERLY_ATOM = new OtpErlangAtom(ERLYBERLY);

    private static final OtpErlangAtom REX_ATOM = atom("rex");

    public static final OtpErlangAtom OK_ATOM = atom("ok");

    private static final OtpErlangAtom MODULE_ATOM = new OtpErlangAtom("module");

    private static final String ERLYBERLY_ERL_PATH = "/erlyberly/beam/erlyberly.erl";

    private static final String ERLYBERLY_BEAM_PATH = "/erlyberly/beam/erlyberly.beam";

    private static final String ERLYBERLY_REMOTE_ERL_PATH = "/tmp/erlyberly.erl";

    private static final int ERL_SIZE_LIMIT = 1024 * 50;

    private static final int BEAM_SIZE_LIMIT = 1024 * 50;

    private static final AtomicLong CHECK_ALIVE_THREAD_COUNTER = new AtomicLong();

    private final TraceManager traceManager;

    private final SimpleBooleanProperty connectedProperty;

    private final SimpleBooleanProperty xrefStartedProperty;

    private final SimpleStringProperty summary;

    private OtpConn connection;

    private final OtpSelfNode self;

    private String remoteNodeName;

    private String cookie;

    private volatile Thread checkAliveThread;

    private final SimpleObjectProperty<AppProcs> appProcs;

    private OtpMbox mbox;

    private volatile boolean connected = false;

    private final ObservableList<OtpErlangObject> crashReports = FXCollections.observableArrayList();

    private boolean manuallyDisconnected = false;

    private RpcCallback<OtpErlangTuple> moduleLoadedCallback;

    private final RecordManager recordManager = new RecordManager();

    /**
     * Called when a trace log is received.
     * <br/>
     * Should only accessed from the FX thread.
     */
    private RpcCallback<TraceLog> traceLogCallback;

    /**
     * When tracing is paused, NodeAPI will stop all traces. When tracing is un-suspended
     * the DbgController must reapply all the traces.
     */
    private final SimpleBooleanProperty suspendedProperty;

    public NodeAPI() {
        traceManager = new TraceManager();

        connectedProperty = new SimpleBooleanProperty();
        connectedProperty.addListener(new ChangeListener<Boolean>() {
            @Override
            public void changed(ObservableValue<? extends Boolean> obv, Boolean o, Boolean n) {
                connected = n;
                if(!n) {
                    xrefStartedProperty.set(false);
                }
            }});
        summary = new SimpleStringProperty("erlyberly not connected");

        appProcs = new SimpleObjectProperty<AppProcs>(new AppProcs(0, LocalDateTime.now()));

        connectedProperty.addListener(this::summaryUpdater);

        xrefStartedProperty = new SimpleBooleanProperty(false);

        suspendedProperty = new SimpleBooleanProperty();

        try {
            String ourNodeName = "erlyberly" + new Random().nextInt(9999);
            self = new OtpSelfNode(ourNodeName);
        }
        catch (IOException e) {
            throw new RuntimeException(e);
        }
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
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        // TODO: here, we've cleared (set to false) being Manually/intentionally disconnected.
        manuallyDisconnected = false;
        connect();
    }

    public synchronized void connect() throws IOException, OtpErlangException, OtpAuthException {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;

        // clear previous connections and threads if any, before we reconnect
        // TODO: investigate whether we need this ...
        disconnect();
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

        if(checkAliveThread == null) {
            checkAliveThread = new CheckAliveThread();
            checkAliveThread.start();
        }
    }

    public void manuallyDisconnect() throws IOException, OtpErlangException {
        // do not require that this function be run outside of the javafx thread
        // since it is useful to block the app closing while the connections are
        // closing.
        //
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
        // do not require that this function be run outside of the javafx thread
        // since it is useful to block the app closing while the connections are
        // closing.
        try {
            if (connection != null)
                connection.close();
        }
        catch(Exception e) {
            System.out.println(e);
        }
        connection = null;
        connected = false;
        Platform.runLater(() -> {
            suspendedProperty.set(false);
        });
    }

    private synchronized void ensureDbgStarted() throws IOException, OtpErlangException {
        OtpErlangList argList = list(tuple(atom(self.node()), mbox.self()), PrefBind.getMaxTraceQueueLengthConfig());
        OtpErlangObject returnedObject = nodeRPC()
                .blockingRPC(ERLYBERLY_ATOM, atom("ensure_dbg_started"), argList);
        // the return should be {ok, TracerPid}
        // we don't need to store the pid because it is registered
        if(!isSuccessTerm(returnedObject)) {
            throw new RuntimeException(returnedObject.toString());
        }
    }

    private boolean isSuccessTerm(OtpErlangObject term) {
        return OK_ATOM.equals(term) || OtpUtil.isTupleTagged(OK_ATOM, term);
    }

    /**
     * Load all modules on paths where the app directory meets the given regular expression, for
     * example if this property is set in the configuration then all erlang applications containing
     * riak will have their modules loaded e.g. riak_kv, riak_core etc. etc.
     *
     * loadModulesRegex=.*riak.*
     *
     * @see https://github.com/andytill/erlyberly/wiki/Preload-modules
     */
    private void loadModulesOnPath(String regex) throws IOException, OtpErlangException {
        if(regex == null || "".equals(regex))
            return;
        OtpErlangObject rpcResult = nodeRPC().blockingRPC(ERLYBERLY_ATOM, atom("load_modules_on_path"), list(regex));
        // work is done asynchronously so just the pid doing the work is returned.
        assert rpcResult instanceof OtpErlangPid : rpcResult;
    }

    private synchronized void addErrorLoggerHandler() throws IOException, OtpErlangException {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        OtpErlangList args = list(mbox.self());
        OtpErlangObject rpcResult = nodeRPC()
                .blockingRPC(atom("error_logger"), atom("add_report_handler"), list(ERLYBERLY_ATOM, args));
        assert OK_ATOM.equals(rpcResult) : rpcResult;
    }

    private synchronized void removeErrorLoggerHandler() throws IOException, OtpErlangException {
        OtpErlangObject rpcResult = nodeRPC()
                .blockingRPC(atom("error_logger"), atom("delete_report_handler"), list(ERLYBERLY_ATOM));
        assert OK_ATOM.equals(rpcResult) : rpcResult;
    }

    class CheckAliveThread extends Thread {

        public CheckAliveThread() {
            setDaemon(true);
            setName("Erlyberly Check Alive "+ CHECK_ALIVE_THREAD_COUNTER.incrementAndGet());
        }

        @Override
        public void run() {
            while(true) {
                if (!manuallyDisconnected) {
                    ensureAlive();
                }
                mySleep(150);
            }
        }

        private OtpErlangObject receiveRPC(OtpErlangObject receive)
                throws OtpErlangExit, OtpErlangDecodeException, IOException, OtpErlangException {
            if(receive == null)
                return null;
            if(!(receive instanceof OtpErlangTuple))
                throw new RuntimeException("Expected tuple but got " + receive);

            OtpErlangTuple tupleResult = (OtpErlangTuple) receive;
            if(isTupleTagged(ERLYBERLY_TRACE_LOG, tupleResult)) {
                traceLogNotification(tupleResult);
            }
            else if(isTupleTagged(ERLYBERLY_ERROR_REPORT_ATOM, tupleResult)) {
                Platform.runLater(() -> { crashReports.add(tupleResult.elementAt(1)); });
            }
            else if(isTupleTagged(ERLYBERLY_MODULE_RELOADED_ATOM, tupleResult)) {
                Platform.runLater(() -> {
                    if(moduleLoadedCallback != null)
                        moduleLoadedCallback.callback((OtpErlangTuple) tupleResult.elementAt(2));
                });
            }
            else if(isTupleTagged(ERLYBERLY_TRACE_OVERLOAD_ATOM, tupleResult)) {
                Platform.runLater(() -> { suspendedProperty.set(true); });
            }
            else if(!isTupleTagged(REX_ATOM, tupleResult)) {
                throw new RuntimeException("Expected tuple tagged with atom rex but got " + tupleResult);
            }
            OtpErlangObject result = tupleResult.elementAt(1);

            // hack to support certain projects, don't ask...
/*            if(isTupleTagged(BET_SERVICES_MSG_ATOM, result)) {
                result = receiveRPC(timeout);
            } */

            return result;
        }

        private synchronized boolean ensureAlive() {
            try {
                OtpErlangObject receiveResult;
                do {
                    int timeout = 0;
                    receiveResult = mbox.receive(timeout);
                    try {
                        receiveRPC(receiveResult);
                    }
                    catch (IOException e) {
                        e.printStackTrace();
                    }
                } while(receiveResult != null);

                if(connection != null && connection.isAlive())
                    return true;
            }
            catch(OtpErlangExit oee) {
                // an exit is what we're checking for so no need to log it
            }
            catch (OtpErlangException e1) {
                e1.printStackTrace();
            }

            Platform.runLater(() -> { connectedProperty.set(false); });

            while(true) {
                try {
                    if(!manuallyDisconnected){
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
    }

    private void loadRemoteErlyberly() throws IOException, OtpErlangException {
        OtpErlangBinary otpErlangBinary = new OtpErlangBinary(loadBeamFile());

        OtpErlangObject result = nodeRPC().blockingRPC(atom("code"), atom("load_binary"),
                list(ERLYBERLY_ATOM, ERLYBERLY_BEAM_PATH, otpErlangBinary));

        if(result instanceof OtpErlangTuple) {
            OtpErlangObject e0 = ((OtpErlangTuple) result).elementAt(0);

            if(!MODULE_ATOM.equals(e0)) {
                compileAndLoadRemoteErlyberly();
            }
        }
        else {
            compileAndLoadRemoteErlyberly();
        }
    }

    private void compileAndLoadRemoteErlyberly() throws IOException, OtpErlangException {
        OtpErlangString tmpFileName = new OtpErlangString(ERLYBERLY_REMOTE_ERL_PATH);
        OtpErlangBinary erlBin = new OtpErlangBinary(loadErlFile());
        OtpErlangObject result = nodeRPC().blockingRPC(atom("file"), atom("write_file"),
                                                       list(tmpFileName, erlBin));
        if (!(atom("ok").equals(result))) {
            loadErlyberlyError(result);
        }
        String remotePathNoErl = ERLYBERLY_REMOTE_ERL_PATH.replaceFirst(".erl$","");
        result = nodeRPC().blockingRPC(atom("compile"), atom("file"), list(remotePathNoErl, list()));
        if (!(tuple(atom("ok"), ERLYBERLY_ATOM).equals(result))) {
            loadErlyberlyError(result);
        }
        nodeRPC().blockingRPC(atom("file"), atom("delete"), list(ERLYBERLY_REMOTE_ERL_PATH));
    }

    private void loadErlyberlyError(OtpErlangObject result) {
        throw new RuntimeException("error loading the erlyberly module, result was " + result);
    }

    private void unloadRemoteErlyberly() throws IOException, OtpErlangException {
        nodeRPC().blockingRPC(atom("code"), atom("purge"), list(ERLYBERLY_ATOM));
        nodeRPC().blockingRPC(atom("code"), atom("delete"), list(ERLYBERLY_ATOM));
        nodeRPC().blockingRPC(atom("code"), atom("soft_purge"), list(ERLYBERLY_ATOM));
    }

    private void traceLogNotification(OtpErlangTuple receive) {
        Platform.runLater(() -> {
            OtpErlangTuple traceLog = (OtpErlangTuple) receive.elementAt(1);
            List<TraceLog> collatedTraces = traceManager.collateTraceSingle(traceLog);
            if(traceLogCallback != null) {
                for (TraceLog log : collatedTraces) {
                    traceLogCallback.callback(log);
                }
            }
        });
    }

    private static byte[] loadBeamFile() throws IOException {
        return loadFile(ERLYBERLY_BEAM_PATH, BEAM_SIZE_LIMIT);
    }

    private static byte[] loadErlFile() throws IOException {
        return loadFile(ERLYBERLY_ERL_PATH, ERL_SIZE_LIMIT);
    }

    private static byte[] loadFile(String resourcePath, int maxSize) throws IOException {
        InputStream resourceAsStream = OtpUtil.class.getResourceAsStream(resourcePath);

        byte[] b = new byte[maxSize];
        int total = 0;
        int read = 0;

        do {
            total += read;
            read = resourceAsStream.read(b, total, maxSize - total);
        } while (read != -1);

        if(total >= maxSize) {
            throw new RuntimeException(resourcePath.replaceAll("[^/]*/","") + " file is too big");
        }

        return Arrays.copyOf(b, total);
    }

    public synchronized void retrieveProcessInfo(List<ProcInfo> processes) {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        if(connection == null || !connected)
            return;
        OtpErlangObject rpcResult = null;
        try {
            rpcResult = nodeRPC().blockingRPC(ERLYBERLY_ATOM, atom("process_info"), list());
            OtpErlangList received = (OtpErlangList) rpcResult;

            for (OtpErlangObject recv : received) {
                if(recv instanceof OtpErlangList) {
                    OtpErlangList pinfo = (OtpErlangList) recv;
                    Map<Object, Object> propsToMap = OtpUtil.propsToMap(pinfo);
                    processes.add(ProcInfo.toProcessInfo(propsToMap));
                }
            }
            Platform.runLater(() -> { appProcs.set(new AppProcs(processes.size(), LocalDateTime.now())); });
        }
        catch (Exception e) {
            throw new RuntimeException("unexpected result: " + rpcResult, e);
        }
    }

    private void mySleep(int millis) {
        try {
            Thread.sleep(millis);
        }
        catch (InterruptedException e1) {
            e1.printStackTrace();
        }
    }

    public synchronized OtpErlangList requestFunctions() throws Exception {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        OtpErlangObject rpcResult = nodeRPC().blockingRPC(ERLYBERLY_ATOM, atom("module_functions"), list());
        if(rpcResult == null || !(rpcResult instanceof OtpErlangList)) {
            throw new RuntimeException("Result of module_functions call should be a list but got " + rpcResult);
        }
        return (OtpErlangList) rpcResult;
    }

    public synchronized void startTrace(ModFunc mf, int maxQueueLen) throws Exception {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        assert mf.getFuncName() != null : "function name cannot be null";
        OtpErlangObject rpcResult = nodeRPC()
                .blockingRPC(ERLYBERLY_ATOM, atom("start_trace"), toStartTraceFnArgs(mf, maxQueueLen));
        assert isSuccessTerm(rpcResult) : rpcResult;
    }

    public synchronized void stopTrace(ModFunc mf) throws Exception {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        assert mf.getFuncName() != null : "function name cannot be null";
        OtpErlangObject rpcResult = nodeRPC()
                .blockingRPC(ERLYBERLY_ATOM, atom("stop_trace"),
                        list(
                            atom(mf.getModuleName()),
                            atom(mf.getFuncName()),
                            mf.getArity(),
                            mf.getArity()
                        ));
        assert isSuccessTerm(rpcResult) : rpcResult;
        // FIXME return the result for errors
    }

    public synchronized void stopAllTraces() throws IOException, OtpErlangException {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        OtpErlangObject rpcResult = nodeRPC()
                .blockingRPC(ERLYBERLY_ATOM, atom("stop_traces"), list());
        assert isSuccessTerm(rpcResult) : rpcResult;
        // FIXME return the result for errors
    }

    private OtpErlangList toStartTraceFnArgs(ModFunc mf, int maxQueueLen) {
        String node = self.node();
        OtpErlangPid self2 = mbox.self();
        return list(
            tuple(OtpUtil.atom(node), self2),
            atom(mf.getModuleName()),
            atom(mf.getFuncName()),
            mf.getArity(),
            maxQueueLen
        );
    }

    public SimpleBooleanProperty connectedProperty() {
        return connectedProperty;
    }

    public SimpleBooleanProperty xrefStartedProperty() {
        return xrefStartedProperty;
    }

    public synchronized List<TraceLog> collectTraceLogs() throws Exception {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        OtpErlangObject prcResult = nodeRPC()
                .blockingRPC(ERLYBERLY_ATOM, atom("collect_trace_logs"), list());
        if(!isTupleTagged(OK_ATOM, prcResult)) {
            if(prcResult != null) {
                System.out.println(prcResult);
            }
            return new ArrayList<TraceLog>();
        }
        OtpErlangList traceLogs = (OtpErlangList) ((OtpErlangTuple) prcResult).elementAt(1);
        return traceManager.collateTraces(traceLogs);
    }

    private NodeRPC nodeRPC() {
        synchronized (this) {
            return new NodeRPC(self, connection);
        }
    }

    private NodeRPC nodeRPC(int timeoutMillis) {
        return new NodeRPC(self, connection, timeoutMillis);
    }

    public synchronized List<SeqTraceLog> collectSeqTraceLogs() throws Exception {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        OtpErlangObject rpcResult = nodeRPC()
                .blockingRPC(ERLYBERLY_ATOM, atom("collect_seq_trace_logs"), list());
        if(!isTupleTagged(OK_ATOM, rpcResult)) {
            return new ArrayList<SeqTraceLog>();
        }

        ArrayList<SeqTraceLog> seqLogs = new ArrayList<SeqTraceLog>();

        try {
            OtpErlangList traceLogs = (OtpErlangList) ((OtpErlangTuple) rpcResult)
                    .elementAt(1);
            for (OtpErlangObject otpErlangObject : traceLogs) {
                seqLogs.add(SeqTraceLog.build(OtpUtil
                        .propsToMap((OtpErlangList) otpErlangObject)));
            }
        }
        catch (ClassCastException e) {
            System.out.println("did not understand result from collect_seq_trace_logs " + rpcResult);
            e.printStackTrace();
        }
        return seqLogs;
    }

    public ObservableValue<? extends String> summaryProperty() {
        return summary;
    }

    private void summaryUpdater(Observable o, Boolean wasConnected, Boolean isConnected) {
        String summaryText = ERLYBERLY;

        OtpSelfNode self2 = self;

        if(self2 != null && !wasConnected && isConnected)
            summaryText = self2.node() + " connected to " + this.remoteNodeName;
        else if(wasConnected && !isConnected)
            summaryText = "erlyberly, connection lost.  reconnecting...";

        summary.set(summaryText);
    }

    public synchronized void seqTrace(ModFunc mf) throws IOException, OtpErlangException {
        OtpErlangObject result = nodeRPC().blockingRPC(ERLYBERLY_ATOM, atom("seq_trace"),
            list(
                tuple(OtpUtil.atom(self.node()), mbox.self()),
                atom(mf.getModuleName()),
                atom(mf.getFuncName()),
                mf.getArity(),
                new OtpErlangAtom(mf.isExported())
            ));
        System.out.println(result);
    }

    public synchronized OtpErlangObject getProcessState(String pidString) throws IOException, OtpErlangException {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        OtpErlangObject result = nodeRPC().blockingRPC(ERLYBERLY_ATOM, atom("get_process_state"), list(pidString));
        if(isTupleTagged(OK_ATOM, result)) {
            return ((OtpErlangTuple)result).elementAt(1);
        }
        // FIXME check result for errors
        return null;
    }

    public synchronized Map<Object, Object> erlangMemory() throws IOException, OtpErlangException {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        OtpErlangObject result = nodeRPC().blockingRPC(atom("erlang"), atom("memory"), list());
        assert result instanceof OtpErlangList : result;
        return OtpUtil.propsToMap((OtpErlangList) result);
    }

    public boolean isConnected() {
        return connected;
    }

    public boolean manuallyDisconnected(){
        return manuallyDisconnected;
    }

    /**
     * Returns {ok, Call_graph}.
     */
    public synchronized OtpErlangObject callGraph(OtpErlangList skippedModuleAtoms, OtpErlangAtom module, OtpErlangAtom function, OtpErlangLong arity) throws IOException, OtpErlangException {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        OtpErlangObject result = nodeRPC()
                .blockingRPC(ERLYBERLY_ATOM, atom("xref_analysis"), list(skippedModuleAtoms, module, function, arity));
        assert isSuccessTerm(result) : result;
        return result;
    }

    /**
     * Start xref but
     */
    public synchronized void ensureXRefStarted() throws IOException, OtpErlangException {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        OtpErlangObject rpcResult = nodeRPC(1000 * 60 * 2)
                .blockingRPC(ERLYBERLY_ATOM, atom("ensure_xref_started"), list());
        assert isSuccessTerm(rpcResult) : "Unexpected erlyberly:ensure_xref_started/0 result " + rpcResult;
        Platform.runLater(() -> { xrefStartedProperty.set(true); });
    }

    private OtpErlangTuple mfaTuple(String module, String function, Integer arity) {
        return tuple(atom(module), atom(function), arity);
    }

    public synchronized String moduleFunctionSourceCode(String module, String function, Integer arity) throws IOException, OtpErlangException {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        OtpErlangObject result = nodeRPC().blockingRPC(ERLYBERLY_ATOM, atom("get_source_code"), list(mfaTuple(module, function, arity)));
        return returnCode(result, "Failed to get source code for " + module + ":" + function + "/" + arity.toString() + ".");
    }

    public synchronized String moduleFunctionSourceCode(String module) throws IOException, OtpErlangException {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        OtpErlangObject result = nodeRPC().blockingRPC(ERLYBERLY_ATOM, atom("get_source_code"), list(atom(module)));
        return returnCode(result, "Failed to get source code for " + module + ".");
    }

    public synchronized String moduleFunctionAbstractCode(String module, String function, Integer arity) throws IOException, OtpErlangException {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        OtpErlangObject result = nodeRPC().blockingRPC(ERLYBERLY_ATOM, atom("get_abstract_code"), list(mfaTuple(module, function, arity)));
        return returnCode(result, "Failed to get abstract code for " + module + ".");
    }

    public synchronized String moduleFunctionAbstractCode(String module) throws IOException, OtpErlangException {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        OtpErlangObject result = nodeRPC().blockingRPC(ERLYBERLY_ATOM, atom("get_abstract_code"), list(atom(module)));
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
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        OtpErlangObject result = nodeRPC().blockingRPC(atom("dict"), atom("to_list"), list(dict));
        assert result instanceof OtpErlangList : result;
        return (OtpErlangList) result;
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
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        OtpErlangObject result = nodeRPC().blockingRPC(ERLYBERLY_ATOM, atom("saleyn_fun_src"), list(fun));
        if(!(result instanceof OtpErlangString))
            throw new OtpErlangException(Objects.toString(result));
        OtpErlangString otpString = (OtpErlangString) result;
        return otpString.stringValue();
    }

    /**
     * Attempt to load the given module name into the attached vm. The trace applied
     * to the code module will see the loaded module and send a message to erlyberly,
     * which will display it in the tree of modules.
     */
    public void tryLoadModule(String moduleNameAtom) throws OtpErlangException, IOException {
        assert moduleNameAtom != null : "module name string is null";
        assert !"".equals(moduleNameAtom) : " module name string is empty";
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        OtpErlangObject result = nodeRPC()
                .blockingRPC(ERLYBERLY_ATOM, TRY_LOAD_MODULE_ATOM, list(atom(moduleNameAtom)));
        assert isTupleTagged(MODULE_ATOM, result) || isTupleTagged(ERROR_ATOM, result) : result;
    }


    public void loadModuleRecords(OtpErlangAtom moduleName) throws OtpErlangException, IOException {
        if(recordManager.isModuleManaged(moduleName))
            return;
        OtpErlangObject result = nodeRPC()
                .blockingRPC(ERLYBERLY_ATOM, atom("load_module_records"), list(moduleName));
        assert isTupleTagged(OK_ATOM, result) : result;
        OtpErlangTuple resultTuple = (OtpErlangTuple) result;
        OtpErlangList records = (OtpErlangList) resultTuple.elementAt(1);
        for (OtpErlangObject obj : records) {
            OtpErlangTuple record = (OtpErlangTuple) obj;
            OtpErlangAtom recordName = (OtpErlangAtom) record.elementAt(0);
            OtpErlangList fieldNameAtoms = (OtpErlangList) record.elementAt(1);
            ArrayList<String> recordNames = new ArrayList<>();
            for (OtpErlangObject nameObj : fieldNameAtoms) {
                OtpErlangAtom nameAtom = (OtpErlangAtom) nameObj;
                recordNames.add(nameAtom.atomValue());
            }
            recordManager.put(new RecordManager.RecordKey(moduleName, recordName), recordNames);
        }
    }

    public RpcCallback<TraceLog> getTraceLogCallback() {
        return traceLogCallback;
    }

    public void setTraceLogCallback(RpcCallback<TraceLog> traceLogCallback) {
        this.traceLogCallback = traceLogCallback;
    }

    public void toggleSuspended() throws OtpErlangException, IOException {
        assert Platform.isFxApplicationThread();
        if(!isSuspended()) {
            ErlyBerly.runIO(() -> {
                try {
                    stopAllTraces();
                }
                catch (Exception e) {
                    e.printStackTrace();
                }
            });
        }
        suspendedProperty.set(!isSuspended());
    }

    public boolean isSuspended() {
        assert Platform.isFxApplicationThread();
        return suspendedProperty.get();
    }

    public SimpleBooleanProperty suspendedProperty() {
        assert Platform.isFxApplicationThread();
        return suspendedProperty;
    }

    public RecordManager getRecordManager() {
        return recordManager;
    }
}
