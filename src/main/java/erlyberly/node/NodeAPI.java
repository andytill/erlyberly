/**
 * erlyberly, erlang trace debugger
 * Copyright (C) 2016 Andy Till
 * <p>
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * <p>
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * <p>
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package erlyberly.node;

import com.ericsson.otp.erlang.*;
import erlyberly.*;
import javafx.application.Platform;
import javafx.beans.Observable;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.atomic.AtomicLong;

public class NodeAPI {

    private static final OtpErlangAtom TRY_LOAD_MODULE_ATOM = OtpUtil.atom("try_load_module");

    private static final OtpErlangAtom ERROR_ATOM = OtpUtil.atom("error");

    private static final OtpErlangAtom ERLYBERLY_TRACE_OVERLOAD_ATOM = OtpUtil.atom("erlyberly_trace_overload");

    private static final String ERLYBERLY = "erlyberly";

    private static final String CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD = "cannot run this method from the FX thread";

    private static final OtpErlangAtom ERLYBERLY_TRACE_LOG = OtpUtil.atom("erlyberly_trace_log");

    private static final OtpErlangAtom ERLYBERLY_ERROR_REPORT_ATOM = OtpUtil.atom("erlyberly_error_report");

    private static final OtpErlangAtom ERLYBERLY_MODULE_RELOADED_ATOM = OtpUtil.atom("erlyberly_module_loaded");

    private static final OtpErlangAtom ERLYBERLY_ATOM = new OtpErlangAtom(ERLYBERLY);

    private static final OtpErlangAtom REX_ATOM = OtpUtil.atom("rex");

    public static final OtpErlangAtom OK_ATOM = OtpUtil.atom("ok");

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

    private volatile boolean connected;

    private final ObservableList<OtpErlangObject> crashReports = FXCollections.observableArrayList();

    private boolean manuallyDisconnected;

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
        super();
        this.traceManager = new TraceManager();

        this.connectedProperty = new SimpleBooleanProperty();
        this.connectedProperty.addListener(new ChangeListener<Boolean>() {
            @Override
            public void changed(final ObservableValue<? extends Boolean> obv, final Boolean o, final Boolean n) {
                NodeAPI.this.connected = n.booleanValue();
                if (!n.booleanValue()) {
                    NodeAPI.this.xrefStartedProperty.set(false);
                }
            }
        });
        this.summary = new SimpleStringProperty("erlyberly not connected");

        this.appProcs = new SimpleObjectProperty<>(new AppProcs(0, LocalDateTime.now()));

        this.connectedProperty.addListener(this::summaryUpdater);

        this.xrefStartedProperty = new SimpleBooleanProperty(false);

        this.suspendedProperty = new SimpleBooleanProperty();

        try {
            final String ourNodeName = "erlyberly" + new Random().nextInt(9999);
            this.self = new OtpSelfNode(ourNodeName);
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
    }

    public NodeAPI connectionInfo(final String remoteNodeName, final String cookie) {
        this.remoteNodeName = remoteNodeName;
        this.cookie = cookie;

        return this;
    }

    public ObservableList<OtpErlangObject> getCrashReports() {
        return this.crashReports;
    }

    public SimpleObjectProperty<AppProcs> appProcsProperty() {
        return this.appProcs;
    }

    public synchronized void manualConnect() throws IOException, OtpErlangException, OtpAuthException {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        // TODO: here, we've cleared (set to false) being Manually/intentionally disconnected.
        this.manuallyDisconnected = false;
        this.connect();
    }

    private synchronized void connect() throws IOException, OtpErlangException, OtpAuthException {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;

        // clear previous connections and threads if any, before we reconnect
        // TODO: investigate whether we need this ...
        this.disconnect();
        if (!this.cookie.isEmpty()) {
            this.self.setCookie(this.cookie);
        }

        // if the node name does not contain a host then assume it is on the
        // same machine
        if (!this.remoteNodeName.contains("@")) {
            final String[] split = this.self.toString().split("@");

            this.remoteNodeName += "@" + split[1];
        }

        this.connection = this.self.connect(new OtpPeer(this.remoteNodeName));

        this.mbox = this.self.createMbox();

        this.loadRemoteErlyberly();

        this.addErrorLoggerHandler();

        // start dbg so we can listen for module loads
        this.ensureDbgStarted();

        this.loadModulesOnPath(PrefBind.getOrDefault("loadModulesRegex", "").toString());

        Platform.runLater(() -> this.connectedProperty.set(true));

        if (null == this.checkAliveThread) {
            this.checkAliveThread = new CheckAliveThread();
            this.checkAliveThread.start();
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
        this.manuallyDisconnected = true;
        this.stopAllTraces();
        this.removeErrorLoggerHandler();
        this.unloadRemoteErlyberly();
        this.mbox.close();
        Platform.runLater(() -> this.connectedProperty.set(false));
    }

    public void disconnect() {
        // do not require that this function be run outside of the javafx thread
        // since it is useful to block the app closing while the connections are
        // closing.
        try {
            if (null != this.connection) this.connection.close();
        } catch (final Exception e) {
            System.out.println(e);
        }
        this.connection = null;
        this.connected = false;
        Platform.runLater(() -> this.suspendedProperty.set(false));
    }

    private synchronized void ensureDbgStarted() throws IOException, OtpErlangException {
        final OtpErlangList argList = OtpUtil.list(OtpUtil.tuple(OtpUtil.atom(this.self.node()), this.mbox.self()), PrefBind.getMaxTraceQueueLengthConfig());
        final OtpErlangObject returnedObject = this.nodeRPC().blockingRPC(ERLYBERLY_ATOM, OtpUtil.atom("ensure_dbg_started"), argList);
        // the return should be {ok, TracerPid}
        // we don't need to store the pid because it is registered
        if (!NodeAPI.isSuccessTerm(returnedObject)) {
            throw new RuntimeException(returnedObject.toString());
        }
    }

    private static boolean isSuccessTerm(final OtpErlangObject term) {
        return OK_ATOM.equals(term) || OtpUtil.isTupleTagged(OK_ATOM, term);
    }

    /**
     * Load all modules on paths where the app directory meets the given regular expression, for
     * example if this property is set in the configuration then all erlang applications containing
     * riak will have their modules loaded e.g. riak_kv, riak_core etc. etc.
     * <p>
     * loadModulesRegex=.*riak.*
     *
     * @see https://github.com/andytill/erlyberly/wiki/Preload-modules
     */
    private void loadModulesOnPath(final String regex) throws IOException, OtpErlangException {
        if (null == regex || regex.isEmpty()) return;
        final OtpErlangObject rpcResult = this.nodeRPC().blockingRPC(ERLYBERLY_ATOM, OtpUtil.atom("load_modules_on_path"), OtpUtil.list(regex));
        // work is done asynchronously so just the pid doing the work is returned.
        assert rpcResult instanceof OtpErlangPid : rpcResult;
    }

    private synchronized void addErrorLoggerHandler() throws IOException, OtpErlangException {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        final OtpErlangList args = OtpUtil.list(this.mbox.self());
        final OtpErlangObject rpcResult = this.nodeRPC().blockingRPC(OtpUtil.atom("error_logger"), OtpUtil.atom("add_report_handler"), OtpUtil.list(ERLYBERLY_ATOM, args));
        assert OK_ATOM.equals(rpcResult) : rpcResult;
    }

    private synchronized void removeErrorLoggerHandler() throws IOException, OtpErlangException {
        final OtpErlangObject rpcResult = this.nodeRPC().blockingRPC(OtpUtil.atom("error_logger"), OtpUtil.atom("delete_report_handler"), OtpUtil.list(ERLYBERLY_ATOM));
        assert OK_ATOM.equals(rpcResult) : rpcResult;
    }

    class CheckAliveThread extends Thread {

        CheckAliveThread() {
            super();
            this.setDaemon(true);
            this.setName("Erlyberly Check Alive " + CHECK_ALIVE_THREAD_COUNTER.incrementAndGet());
        }

        @Override
        public void run() {
            while (true) {
                if (!NodeAPI.this.manuallyDisconnected) {
                    this.ensureAlive();
                }
                NodeAPI.mySleep(150);
            }
        }

        private void receiveRPC(final OtpErlangObject receive) throws IOException, OtpErlangException {
            if (null == receive) return;
            if (!(receive instanceof OtpErlangTuple)) throw new RuntimeException("Expected tuple but got " + receive);

            final OtpErlangTuple tupleResult = (OtpErlangTuple) receive;
            if (OtpUtil.isTupleTagged(ERLYBERLY_TRACE_LOG, tupleResult)) {
                NodeAPI.this.traceLogNotification(tupleResult);
            } else if (OtpUtil.isTupleTagged(ERLYBERLY_ERROR_REPORT_ATOM, tupleResult)) {
                Platform.runLater(() -> NodeAPI.this.crashReports.add(tupleResult.elementAt(1)));
            } else if (OtpUtil.isTupleTagged(ERLYBERLY_MODULE_RELOADED_ATOM, tupleResult)) {
                Platform.runLater(() -> {
                    if (null != NodeAPI.this.moduleLoadedCallback)
                        NodeAPI.this.moduleLoadedCallback.callback((OtpErlangTuple) tupleResult.elementAt(2));
                });
            } else if (OtpUtil.isTupleTagged(ERLYBERLY_TRACE_OVERLOAD_ATOM, tupleResult)) {
                Platform.runLater(() -> NodeAPI.this.suspendedProperty.set(true));
            } else if (!OtpUtil.isTupleTagged(REX_ATOM, tupleResult)) {
                throw new RuntimeException("Expected tuple tagged with atom rex but got " + tupleResult);
            }
            final OtpErlangObject result = tupleResult.elementAt(1);

            // hack to support certain projects, don't ask...
/*            if(isTupleTagged(BET_SERVICES_MSG_ATOM, result)) {
                result = receiveRPC(timeout);
            } */

        }

        private synchronized void ensureAlive() {
            try {
                OtpErlangObject receiveResult;
                do {
                    final int timeout = 0;
                    receiveResult = NodeAPI.this.mbox.receive(timeout);
                    try {
                        this.receiveRPC(receiveResult);
                    } catch (final IOException e) {
                        e.printStackTrace();
                    }
                } while (null != receiveResult);

                if (null != NodeAPI.this.connection && NodeAPI.this.connection.isAlive()) return;
            } catch (final OtpErlangExit oee) {
                // an exit is what we're checking for so no need to log it
            } catch (final OtpErlangException e1) {
                e1.printStackTrace();
            }

            Platform.runLater(() -> NodeAPI.this.connectedProperty.set(false));

            while (true) {
                try {
                    if (!NodeAPI.this.manuallyDisconnected) {
                        NodeAPI.this.connect();
                        break;
                    }
                } catch (final Exception e) {
                    final int millis = 50;
                    NodeAPI.mySleep(millis);

                }
            }
        }
    }

    private void loadRemoteErlyberly() throws IOException, OtpErlangException {
        final OtpErlangBinary otpErlangBinary = new OtpErlangBinary(loadBeamFile());

        final OtpErlangObject result = this.nodeRPC().blockingRPC(OtpUtil.atom("code"), OtpUtil.atom("load_binary"), OtpUtil.list(ERLYBERLY_ATOM, ERLYBERLY_BEAM_PATH, otpErlangBinary));

        if (result instanceof OtpErlangTuple) {
            final OtpErlangObject e0 = ((OtpErlangTuple) result).elementAt(0);

            if (!MODULE_ATOM.equals(e0)) {
                this.compileAndLoadRemoteErlyberly();
            }
        } else {
            this.compileAndLoadRemoteErlyberly();
        }
    }

    private void compileAndLoadRemoteErlyberly() throws IOException, OtpErlangException {
        final OtpErlangString tmpFileName = new OtpErlangString(ERLYBERLY_REMOTE_ERL_PATH);
        final OtpErlangBinary erlBin = new OtpErlangBinary(loadErlFile());
        OtpErlangObject result = this.nodeRPC().blockingRPC(OtpUtil.atom("file"), OtpUtil.atom("write_file"), OtpUtil.list(tmpFileName, erlBin));
        if (!(OtpUtil.atom("ok").equals(result))) {
            NodeAPI.loadErlyberlyError(result);
        }
        final String remotePathNoErl = ERLYBERLY_REMOTE_ERL_PATH.replaceFirst(".erl$", "");
        result = this.nodeRPC().blockingRPC(OtpUtil.atom("compile"), OtpUtil.atom("file"), OtpUtil.list(remotePathNoErl, OtpUtil.list(OtpUtil.atom("binary"))));
        if (!(result instanceof OtpErlangTuple && OtpUtil.atom("ok").equals(((OtpErlangTuple) result).elementAt(0)))) {
            NodeAPI.loadErlyberlyError(result);
        }
        final OtpErlangBinary beamBin = (OtpErlangBinary) ((OtpErlangTuple) result).elementAt(2);
        result = this.nodeRPC().blockingRPC(OtpUtil.atom("code"), OtpUtil.atom("load_binary"), OtpUtil.list(ERLYBERLY_ATOM, ERLYBERLY_ATOM, beamBin));
        if (!OtpUtil.tuple(OtpUtil.atom("module"), ERLYBERLY_ATOM).equals(result)) {
            NodeAPI.loadErlyberlyError(result);
        }
        this.nodeRPC().blockingRPC(OtpUtil.atom("file"), OtpUtil.atom("delete"), OtpUtil.list(ERLYBERLY_REMOTE_ERL_PATH));
    }

    private static void loadErlyberlyError(final OtpErlangObject result) {
        throw new RuntimeException("error loading the erlyberly module, result was " + result);
    }

    private void unloadRemoteErlyberly() throws IOException, OtpErlangException {
        this.nodeRPC().blockingRPC(OtpUtil.atom("code"), OtpUtil.atom("purge"), OtpUtil.list(ERLYBERLY_ATOM));
        this.nodeRPC().blockingRPC(OtpUtil.atom("code"), OtpUtil.atom("delete"), OtpUtil.list(ERLYBERLY_ATOM));
        this.nodeRPC().blockingRPC(OtpUtil.atom("code"), OtpUtil.atom("soft_purge"), OtpUtil.list(ERLYBERLY_ATOM));
    }

    private void traceLogNotification(final OtpErlangTuple receive) {
        Platform.runLater(() -> {
            final OtpErlangTuple traceLog = (OtpErlangTuple) receive.elementAt(1);
            final List<TraceLog> collatedTraces = this.traceManager.collateTraceSingle(traceLog);
            if (null != this.traceLogCallback) {
                for (final TraceLog log : collatedTraces) {
                    this.traceLogCallback.callback(log);
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

    private static byte[] loadFile(final String resourcePath, final int maxSize) throws IOException {
        final InputStream resourceAsStream = OtpUtil.class.getResourceAsStream(resourcePath);

        final byte[] b = new byte[maxSize];
        int total = 0;
        int read = 0;

        do {
            total += read;
            assert null != resourceAsStream;
            read = resourceAsStream.read(b, total, maxSize - total);
        } while (-1 != read);

        if (total >= maxSize) {
            throw new RuntimeException(resourcePath.replaceAll("[^/]*/", "") + " file is too big");
        }

        return Arrays.copyOf(b, total);
    }

    public synchronized void retrieveProcessInfo(final List<ProcInfo> processes) {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        if (null == this.connection || !this.connected) return;
        OtpErlangObject rpcResult = null;
        try {
            rpcResult = this.nodeRPC().blockingRPC(ERLYBERLY_ATOM, OtpUtil.atom("process_info"), OtpUtil.list());
            final OtpErlangList received = (OtpErlangList) rpcResult;

            for (final OtpErlangObject recv : received) {
                if (recv instanceof OtpErlangList) {
                    final OtpErlangList pinfo = (OtpErlangList) recv;
                    final Map<Object, Object> propsToMap = OtpUtil.propsToMap(pinfo);
                    processes.add(ProcInfo.toProcessInfo(propsToMap));
                }
            }
            Platform.runLater(() -> this.appProcs.set(new AppProcs(processes.size(), LocalDateTime.now())));
        } catch (final Exception e) {
            throw new RuntimeException("unexpected result: " + rpcResult, e);
        }
    }

    private static void mySleep(final int millis) {
        try {
            Thread.sleep(millis);
        } catch (final InterruptedException e1) {
            e1.printStackTrace();
        }
    }

    public synchronized OtpErlangList requestFunctions() throws Exception {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        final OtpErlangObject rpcResult = this.nodeRPC().blockingRPC(ERLYBERLY_ATOM, OtpUtil.atom("module_functions"), OtpUtil.list());
        if (!(rpcResult instanceof OtpErlangList)) {
            throw new RuntimeException("Result of module_functions call should be a list but got " + rpcResult);
        }
        return (OtpErlangList) rpcResult;
    }

    public synchronized void startTrace(final ModFunc mf, final int maxQueueLen) throws Exception {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        assert null != mf.getFuncName() : "function name cannot be null";
        final OtpErlangObject rpcResult = this.nodeRPC().blockingRPC(ERLYBERLY_ATOM, OtpUtil.atom("start_trace"), this.toStartTraceFnArgs(mf, maxQueueLen));
        assert NodeAPI.isSuccessTerm(rpcResult) : rpcResult;
    }

    public synchronized void stopTrace(final ModFunc mf) throws Exception {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        assert null != mf.getFuncName() : "function name cannot be null";
        final OtpErlangObject rpcResult = this.nodeRPC().blockingRPC(ERLYBERLY_ATOM, OtpUtil.atom("stop_trace"), OtpUtil.list(OtpUtil.atom(mf.getModuleName()), OtpUtil.atom(mf.getFuncName()), mf.getArity(), mf.getArity()));
        assert NodeAPI.isSuccessTerm(rpcResult) : rpcResult;
        // FIXME return the result for errors
    }

    private synchronized void stopAllTraces() throws IOException, OtpErlangException {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        final OtpErlangObject rpcResult = this.nodeRPC().blockingRPC(ERLYBERLY_ATOM, OtpUtil.atom("stop_traces"), OtpUtil.list());
        assert NodeAPI.isSuccessTerm(rpcResult) : rpcResult;
        // FIXME return the result for errors
    }

    private OtpErlangList toStartTraceFnArgs(final ModFunc mf, final int maxQueueLen) {
        final String node = this.self.node();
        final OtpErlangPid self2 = this.mbox.self();
        return OtpUtil.list(OtpUtil.tuple(OtpUtil.atom(node), self2), OtpUtil.atom(mf.getModuleName()), OtpUtil.atom(mf.getFuncName()), mf.getArity(), maxQueueLen);
    }

    public SimpleBooleanProperty connectedProperty() {
        return this.connectedProperty;
    }

    public SimpleBooleanProperty xrefStartedProperty() {
        return this.xrefStartedProperty;
    }

    public synchronized List<TraceLog> collectTraceLogs() throws Exception {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        final OtpErlangObject prcResult = this.nodeRPC().blockingRPC(ERLYBERLY_ATOM, OtpUtil.atom("collect_trace_logs"), OtpUtil.list());
        if (!OtpUtil.isTupleTagged(OK_ATOM, prcResult)) {
            if (null != prcResult) {
                System.out.println(prcResult);
            }
            return new ArrayList<>();
        }
        final OtpErlangList traceLogs = (OtpErlangList) ((OtpErlangTuple) prcResult).elementAt(1);
        return this.traceManager.collateTraces(traceLogs);
    }

    private synchronized NodeRPC nodeRPC() {
        return new NodeRPC(this.self, this.connection);
    }

    private NodeRPC nodeRPC(final int timeoutMillis) {
        return new NodeRPC(this.self, this.connection, timeoutMillis);
    }

    public synchronized List<SeqTraceLog> collectSeqTraceLogs() throws Exception {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        final OtpErlangObject rpcResult = this.nodeRPC().blockingRPC(ERLYBERLY_ATOM, OtpUtil.atom("collect_seq_trace_logs"), OtpUtil.list());
        if (!OtpUtil.isTupleTagged(OK_ATOM, rpcResult)) {
            return new ArrayList<>();
        }

        final List<SeqTraceLog> seqLogs = new ArrayList<>();

        try {
            final OtpErlangList traceLogs = (OtpErlangList) ((OtpErlangTuple) rpcResult).elementAt(1);
            for (final OtpErlangObject otpErlangObject : traceLogs) {
                seqLogs.add(SeqTraceLog.build(OtpUtil.propsToMap((OtpErlangList) otpErlangObject)));
            }
        } catch (final ClassCastException e) {
            System.out.println("did not understand result from collect_seq_trace_logs " + rpcResult);
            e.printStackTrace();
        }
        return seqLogs;
    }

    public ObservableValue<? extends String> summaryProperty() {
        return this.summary;
    }

    private void summaryUpdater(final Observable o, final Boolean wasConnected, final Boolean isConnected) {
        String summaryText = ERLYBERLY;

        final OtpSelfNode self2 = this.self;

        if (null != self2 && !wasConnected.booleanValue() && isConnected.booleanValue())
            summaryText = self2.node() + " connected to " + this.remoteNodeName;
        else if (wasConnected.booleanValue() && !isConnected.booleanValue())
            summaryText = "erlyberly, connection lost.  reconnecting...";

        this.summary.set(summaryText);
    }

    public synchronized void seqTrace(final ModFunc mf) throws IOException, OtpErlangException {
        final OtpErlangObject result = this.nodeRPC().blockingRPC(ERLYBERLY_ATOM, OtpUtil.atom("seq_trace"), OtpUtil.list(OtpUtil.tuple(OtpUtil.atom(this.self.node()), this.mbox.self()), OtpUtil.atom(mf.getModuleName()), OtpUtil.atom(mf.getFuncName()), mf.getArity(), new OtpErlangAtom(mf.isExported())));
        System.out.println(result);
    }

    public synchronized OtpErlangObject getProcessState(final String pidString) throws IOException, OtpErlangException {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        final OtpErlangObject result = this.nodeRPC().blockingRPC(ERLYBERLY_ATOM, OtpUtil.atom("get_process_state"), OtpUtil.list(pidString));
        if (OtpUtil.isTupleTagged(OK_ATOM, result)) {
            return ((OtpErlangTuple) result).elementAt(1);
        }
        // FIXME check result for errors
        return null;
    }

    public synchronized Map<Object, Object> erlangMemory() throws IOException, OtpErlangException {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        final OtpErlangObject result = this.nodeRPC().blockingRPC(OtpUtil.atom("erlang"), OtpUtil.atom("memory"), OtpUtil.list());
        assert result instanceof OtpErlangList : result;
        return OtpUtil.propsToMap((OtpErlangList) result);
    }

    public boolean isConnected() {
        return this.connected;
    }

    public boolean manuallyDisconnected() {
        return this.manuallyDisconnected;
    }

    /**
     * Returns {ok, Call_graph}.
     */
    public synchronized OtpErlangObject callGraph(final OtpErlangList skippedModuleAtoms, final OtpErlangAtom module, final OtpErlangAtom function, final OtpErlangLong arity) throws IOException, OtpErlangException {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        final OtpErlangObject result = this.nodeRPC().blockingRPC(ERLYBERLY_ATOM, OtpUtil.atom("xref_analysis"), OtpUtil.list(skippedModuleAtoms, module, function, arity));
        assert NodeAPI.isSuccessTerm(result) : result;
        return result;
    }

    /**
     * Start xref but
     */
    public synchronized void ensureXRefStarted() throws IOException, OtpErlangException {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        final OtpErlangObject rpcResult = this.nodeRPC(1000 * 60 * 2).blockingRPC(ERLYBERLY_ATOM, OtpUtil.atom("ensure_xref_started"), OtpUtil.list());
        assert NodeAPI.isSuccessTerm(rpcResult) : "Unexpected erlyberly:ensure_xref_started/0 result " + rpcResult;
        Platform.runLater(() -> this.xrefStartedProperty.set(true));
    }

    private static OtpErlangTuple mfaTuple(final String module, final String function, final Integer arity) {
        return OtpUtil.tuple(OtpUtil.atom(module), OtpUtil.atom(function), arity);
    }

    public synchronized String moduleFunctionSourceCode(final String module, final String function, final Integer arity) throws IOException, OtpErlangException {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        final OtpErlangObject result = this.nodeRPC().blockingRPC(ERLYBERLY_ATOM, OtpUtil.atom("get_source_code"), OtpUtil.list(NodeAPI.mfaTuple(module, function, arity)));
        return NodeAPI.returnCode(result, "Failed to get source code for " + module + ":" + function + "/" + arity.toString() + ".");
    }

    public synchronized String moduleFunctionSourceCode(final String module) throws IOException, OtpErlangException {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        final OtpErlangObject result = this.nodeRPC().blockingRPC(ERLYBERLY_ATOM, OtpUtil.atom("get_source_code"), OtpUtil.list(OtpUtil.atom(module)));
        return NodeAPI.returnCode(result, "Failed to get source code for " + module + ".");
    }

    public synchronized String moduleFunctionAbstractCode(final String module, final String function, final Integer arity) throws IOException, OtpErlangException {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        final OtpErlangObject result = this.nodeRPC().blockingRPC(ERLYBERLY_ATOM, OtpUtil.atom("get_abstract_code"), OtpUtil.list(NodeAPI.mfaTuple(module, function, arity)));
        return NodeAPI.returnCode(result, "Failed to get abstract code for " + module + ".");
    }

    public synchronized String moduleFunctionAbstractCode(final String module) throws IOException, OtpErlangException {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        final OtpErlangObject result = this.nodeRPC().blockingRPC(ERLYBERLY_ATOM, OtpUtil.atom("get_abstract_code"), OtpUtil.list(OtpUtil.atom(module)));
        return NodeAPI.returnCode(result, "Failed to get abstract code for " + module + ".");
    }

    private static String returnCode(final OtpErlangObject result, final String errorResponse) {
        if (OtpUtil.isTupleTagged(OK_ATOM, result)) {
            final OtpErlangBinary bin = (OtpErlangBinary) ((OtpErlangTuple) result).elementAt(1);
            return new String(bin.binaryValue(), StandardCharsets.UTF_8);
        } else {
            final OtpErlangBinary bin = (OtpErlangBinary) ((OtpErlangTuple) result).elementAt(1);
            final String err = new String(bin.binaryValue(), StandardCharsets.UTF_8);
            System.out.println(err);
            return errorResponse;
        }
    }

    public synchronized OtpErlangList dictToPropslist(final OtpErlangObject dict) throws IOException, OtpErlangException {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        final OtpErlangObject result = this.nodeRPC().blockingRPC(OtpUtil.atom("dict"), OtpUtil.atom("to_list"), OtpUtil.list(dict));
        assert result instanceof OtpErlangList : result;
        return (OtpErlangList) result;
    }

    /**
     * Set the callback that is invoked when erlyberly receives a message that a
     * module has been loaded, or reloaded by the VM. The callback argument is in
     * the format {module(), ExportedFuncs, UnexportedFuncs}. A function is the
     * format {atom(), integer()}.
     */
    public void setModuleLoadedCallback(final RpcCallback<OtpErlangTuple> aModuleLoadedCallback) {
        this.moduleLoadedCallback = aModuleLoadedCallback;
    }

    public synchronized String decompileFun(final OtpErlangFun fun) throws IOException, OtpErlangException {
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        final OtpErlangObject result = this.nodeRPC().blockingRPC(ERLYBERLY_ATOM, OtpUtil.atom("saleyn_fun_src"), OtpUtil.list(fun));
        if (!(result instanceof OtpErlangString)) throw new OtpErlangException(Objects.toString(result));
        final OtpErlangString otpString = (OtpErlangString) result;
        return otpString.stringValue();
    }

    /**
     * Attempt to load the given module name into the attached vm. The trace applied
     * to the code module will see the loaded module and send a message to erlyberly,
     * which will display it in the tree of modules.
     */
    public void tryLoadModule(final String moduleNameAtom) throws OtpErlangException, IOException {
        assert null != moduleNameAtom : "module name string is null";
        assert !moduleNameAtom.isEmpty() : " module name string is empty";
        assert !Platform.isFxApplicationThread() : CANNOT_RUN_THIS_METHOD_FROM_THE_FX_THREAD;
        final OtpErlangObject result = this.nodeRPC().blockingRPC(ERLYBERLY_ATOM, TRY_LOAD_MODULE_ATOM, OtpUtil.list(OtpUtil.atom(moduleNameAtom)));
        assert OtpUtil.isTupleTagged(MODULE_ATOM, result) || OtpUtil.isTupleTagged(ERROR_ATOM, result) : result;
    }


    public void loadModuleRecords(final OtpErlangAtom moduleName) throws OtpErlangException, IOException {
        if (this.recordManager.isModuleManaged(moduleName)) return;
        final OtpErlangObject result = this.nodeRPC().blockingRPC(ERLYBERLY_ATOM, OtpUtil.atom("load_module_records"), OtpUtil.list(moduleName));
        assert OtpUtil.isTupleTagged(OK_ATOM, result) : result;
        final OtpErlangTuple resultTuple = (OtpErlangTuple) result;
        final OtpErlangObject objList = resultTuple.elementAt(1);
        OtpErlangList records = new OtpErlangList();
        if (null != objList) {
            try {
                records = (OtpErlangList) objList;
            } catch (final Exception ignored) {

            }
        }
        for (final OtpErlangObject obj : records) {
            final OtpErlangTuple record = (OtpErlangTuple) obj;
            final OtpErlangAtom recordName = (OtpErlangAtom) record.elementAt(0);
            final OtpErlangList fieldNameAtoms = (OtpErlangList) record.elementAt(1);
            final List<String> recordNames = new ArrayList<>();
            for (final OtpErlangObject nameObj : fieldNameAtoms) {
                final OtpErlangAtom nameAtom = (OtpErlangAtom) nameObj;
                recordNames.add(nameAtom.atomValue());
            }
            this.recordManager.put(new RecordManager.RecordKey(moduleName, recordName), recordNames);
        }
    }

    public RpcCallback<TraceLog> getTraceLogCallback() {
        return this.traceLogCallback;
    }

    public void setTraceLogCallback(final RpcCallback<TraceLog> traceLogCallback) {
        this.traceLogCallback = traceLogCallback;
    }

    public void toggleSuspended() throws OtpErlangException, IOException {
        assert Platform.isFxApplicationThread();
        if (!this.isSuspended()) {
            ErlyBerly.runIO(() -> {
                try {
                    this.stopAllTraces();
                } catch (final Exception e) {
                    e.printStackTrace();
                }
            });
        }
        this.suspendedProperty.set(!this.isSuspended());
    }

    public boolean isSuspended() {
        assert Platform.isFxApplicationThread();
        return this.suspendedProperty.get();
    }

    public SimpleBooleanProperty suspendedProperty() {
        assert Platform.isFxApplicationThread();
        return this.suspendedProperty;
    }

    public RecordManager getRecordManager() {
        return this.recordManager;
    }
}
