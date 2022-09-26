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
package erlyberly;

import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangTuple;
import erlyberly.node.OtpUtil;
import erlyberly.node.RpcCallback;
import javafx.application.Platform;
import javafx.beans.InvalidationListener;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.fxml.Initializable;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.ResourceBundle;


public class DbgController implements Initializable {

    /**
     * The maximum trace logs that erlyberly can show in the table before it must
     * load shed them e.g. start throwing them away so that the UI does not attempt
     * to take an unlimited amount of memory, and fail!
     * <p>
     * This is only checked at start up, so if it is changed it requires a restart.
     */
    private static final int MAX_TRACE_LOGS;

    static {
        final Number maxTraceLogs = (Number) PrefBind.getOrDefault("maxTraceLogs", 1000);
        MAX_TRACE_LOGS = maxTraceLogs.intValue();
    }

    private final ObservableList<TraceLog> traceLogs = FXCollections.observableArrayList();

    private final ObservableList<ModFunc> traces = FXCollections.observableArrayList();

    private final ObservableList<SeqTraceLog> seqTraces = FXCollections.observableArrayList();

    private volatile boolean collectingSeqTraces;

    private TraceLog loadSheddingLog;

    @Override
    public void initialize(final URL url, final ResourceBundle r) {
        ErlyBerly.nodeAPI().setTraceLogCallback((traceLog) -> {
            if (this.traceLogs.size() > MAX_TRACE_LOGS) {
                if (null == this.loadSheddingLog) {
                    this.loadSheddingLog = TraceLog.newLoadShedding();
                    this.traceLogs.add(this.loadSheddingLog);
                }
                return;
            }
            if (null != this.loadSheddingLog) {
                this.traceLogs.remove(this.loadSheddingLog);
                this.loadSheddingLog = null;
            }
            this.traceLogs.add(traceLog);
        });
        ErlyBerly.nodeAPI().suspendedProperty().addListener((o, oldv, suspended) -> {
            // an un-suspended is similar to a node coming back online, reapply our known traces
            if (!suspended.booleanValue() && ErlyBerly.nodeAPI().isConnected()) {
                this.reapplyTraces();
            }
        });
        ErlyBerly.nodeAPI().connectedProperty().addListener((o, oldV, newV) -> {
            if (oldV.booleanValue() && !newV.booleanValue()) {
                this.traceLogs.add(TraceLog.newNodeDown());
            }
        });
        new SeqTraceCollectorThread(this.seqTraces::addAll).start();
    }

    public ObservableList<TraceLog> getTraceLogs() {
        return this.traceLogs;
    }

    public ObservableList<SeqTraceLog> getSeqTraceLogs() {
        return this.seqTraces;
    }

    public boolean isTraced(final ModFunc function) {
        return this.traces.contains(function);
    }

    public void toggleTraceModFunc(final ModFunc function) {
        // if tracing is suspended, we can't apply a new trace because that will
        // leave us in a state where some traces are active and others are not
        if (ErlyBerly.nodeAPI().isSuspended()) return;
        if (this.traces.contains(function)) this.onRemoveTracer(null, function);
        else this.traceModFunc(function);
    }

    private void traceModFunc(final ModFunc function) {
        ErlyBerly.runIO(() -> {
            try {
                ErlyBerly.nodeAPI().startTrace(function, PrefBind.getMaxTraceQueueLengthConfig());

                Platform.runLater(() -> this.traces.add(function));

                ErlyBerly.nodeAPI().loadModuleRecords(OtpUtil.atom(function.getModuleName()));
            } catch (final Exception ex) {
                ex.printStackTrace();
            }
        });
    }

    private void onRemoveTracer(final ActionEvent e, final ModFunc function) {
        ErlyBerly.runIO(() -> {
            try {
                ErlyBerly.nodeAPI().stopTrace(function);

                Platform.runLater(() -> this.traces.remove(function));
            } catch (final Exception ex) {
                ex.printStackTrace();
            }
        });
    }

    public void reapplyTraces() {
        final int maxTraceQueueLengthConfig = PrefBind.getMaxTraceQueueLengthConfig();
        final Iterable<ModFunc> tracesCopy = new ArrayList<>(this.traces);
        ErlyBerly.runIO(() -> {
            for (final ModFunc function : tracesCopy) {
                try {
                    ErlyBerly.nodeAPI().startTrace(function, maxTraceQueueLengthConfig);
                } catch (final Exception e) {
                    e.printStackTrace();
                    Platform.runLater(() -> this.traces.remove(function));
                }
            }
        });
    }

    public void addTraceListener(final InvalidationListener listener) {
        this.traces.addListener(listener);
    }

    public static void requestModFuncs(final RpcCallback<OtpErlangList> rpcCallback) {
        new GetModulesThread(rpcCallback).start();
    }

    public void seqTrace(final ModFunc value) {
        try {
            ErlyBerly.nodeAPI().seqTrace(value);
            this.collectingSeqTraces = true;
        } catch (final OtpErlangException | IOException e) {
            e.printStackTrace();
        }
    }

    class SeqTraceCollectorThread extends Thread {
        private final RpcCallback<SeqTraceLog> callback;

        SeqTraceCollectorThread(final RpcCallback<SeqTraceLog> aCallback) {
            super();
            this.callback = aCallback;

            this.setDaemon(true);
            this.setName("Erlyberly Collect Seq Traces");
        }

        @Override
        public void run() {
            while (true) {
                if (DbgController.this.collectingSeqTraces && ErlyBerly.nodeAPI().isConnected()) {
                    try {
                        final List<SeqTraceLog> seqTraceLogs = ErlyBerly.nodeAPI().collectSeqTraceLogs();

                        for (final SeqTraceLog seqTraceLog : seqTraceLogs) {
                            Platform.runLater(() -> this.callback.callback(seqTraceLog));
                        }
                    } catch (final Exception e) {
                        e.printStackTrace();
                    }
                }

                try {
                    Thread.sleep(100);
                } catch (final InterruptedException e) {
                    e.printStackTrace();
                }
            }
        }
    }

    static class GetModulesThread extends Thread {
        private final RpcCallback<OtpErlangList> rpcCallback;

        GetModulesThread(final RpcCallback<OtpErlangList> anRpcCallback) {
            super();
            this.rpcCallback = anRpcCallback;

            this.setDaemon(true);
            this.setName("Erlyberly Get Modules");
        }

        @Override
        public void run() {
            try {
                final OtpErlangList functions = ErlyBerly.nodeAPI().requestFunctions();
                Platform.runLater(() -> this.rpcCallback.callback(functions));
            } catch (final Exception e) {
                e.printStackTrace();
            }
        }
    }

    public static String moduleFunctionSourceCode(final String module, final String function, final Integer arity) throws IOException, OtpErlangException {
        return ErlyBerly.nodeAPI().moduleFunctionSourceCode(module, function, arity);
    }

    public static String moduleFunctionSourceCode(final String module) throws IOException, OtpErlangException {
        return ErlyBerly.nodeAPI().moduleFunctionSourceCode(module);
    }

    public static String moduleFunctionAbstCode(final String module) throws IOException, OtpErlangException {
        return ErlyBerly.nodeAPI().moduleFunctionAbstractCode(module);
    }

    public static String moduleFunctionAbstCode(final String module, final String function, final Integer arity) throws IOException, OtpErlangException {
        return ErlyBerly.nodeAPI().moduleFunctionAbstractCode(module, function, arity);
    }

    public static void setModuleLoadedCallback(final RpcCallback<OtpErlangTuple> moduleLoadedCallback) {
        ErlyBerly.nodeAPI().setModuleLoadedCallback(moduleLoadedCallback);
    }

}
