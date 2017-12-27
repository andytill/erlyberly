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
package erlyberly;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.ResourceBundle;

import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlyberly.node.RpcCallback;
import javafx.application.Platform;
import javafx.beans.InvalidationListener;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.fxml.Initializable;


public class DbgController implements Initializable {

    /**
     * The maximum trace logs that erlyberly can show in the table before it must
     * load shed them e.g. start throwing them away so that the UI does not attempt
     * to take an unlimited amount of memory, and fail!
     *
     * This is only checked at start up, so if it is changed it requires a restart.
     */
    private static final int MAX_TRACE_LOGS;

    static {
        Number maxTraceLogs = (Number) PrefBind.getOrDefault("maxTraceLogs", 1000);
        MAX_TRACE_LOGS = maxTraceLogs.intValue();
    }

    private final ObservableList<TraceLog> traceLogs = FXCollections.observableArrayList();

    private final ObservableList<ModFunc> traces = FXCollections.observableArrayList();

    private final ObservableList<SeqTraceLog> seqTraces = FXCollections.observableArrayList();

    private volatile boolean collectingSeqTraces;

    private TraceLog loadSheddingLog;

    @Override
    public void initialize(URL url, ResourceBundle r) {
        ErlyBerly.nodeAPI().setTraceLogCallback((traceLog) -> {
            if(traceLogs.size() > MAX_TRACE_LOGS) {
                if(loadSheddingLog == null) {
                    loadSheddingLog = TraceLog.newLoadShedding();
                    traceLogs.add(loadSheddingLog);
                }
                return;
            }
            if(loadSheddingLog != null) {
                traceLogs.remove(loadSheddingLog);
                loadSheddingLog = null;
            }
            traceLogs.add(traceLog);
        });
        ErlyBerly.nodeAPI().suspendedProperty().addListener((o, oldv, suspended) -> {
            // an un-suspended is similar to a node coming back online, reapply our known traces
            if(!suspended && ErlyBerly.nodeAPI().isConnected()) {
                reapplyTraces();
            }
        });
        ErlyBerly.nodeAPI().connectedProperty().addListener(
            (o, oldV, newV) -> {
                if(oldV && !newV) {
                    traceLogs.add(TraceLog.newNodeDown());
                }
            });
        new SeqTraceCollectorThread((seqs) -> { seqTraces.addAll(seqs); }).start();
    }

    public ObservableList<TraceLog> getTraceLogs() {
        return traceLogs;
    }

    public ObservableList<SeqTraceLog> getSeqTraceLogs() {
        return seqTraces;
    }

    public boolean isTraced(ModFunc function) {
        return traces.contains(function);
    }

    public void toggleTraceModFunc(ModFunc function) {
        // if tracing is suspended, we can't apply a new trace because that will
        // leave us in a state where some traces are active and others are not
        if(ErlyBerly.nodeAPI().isSuspended())
            return;
        if(traces.contains(function))
            onRemoveTracer(null, function);
        else
            traceModFunc(function);
    }

    private void traceModFunc(ModFunc function) {
        ErlyBerly.runIO(() -> {
            try {
                ErlyBerly.nodeAPI().startTrace(function, PrefBind.getMaxTraceQueueLengthConfig());

                Platform.runLater(() -> { traces.add(function); });
            }
            catch (Exception ex) {
                ex.printStackTrace();
            }
        });
    }

    private void onRemoveTracer(ActionEvent e, ModFunc function) {
        ErlyBerly.runIO(() -> {
            try {
                ErlyBerly.nodeAPI().stopTrace(function);

                Platform.runLater(() -> { traces.remove(function); });
            }
            catch (Exception ex) {
                ex.printStackTrace();
            }
        });
    }

    public void reapplyTraces() {
        final int maxTraceQueueLengthConfig = PrefBind.getMaxTraceQueueLengthConfig();
        final ArrayList<ModFunc> tracesCopy = new ArrayList<ModFunc>(traces);
        ErlyBerly.runIO(() -> {
            for (ModFunc function : tracesCopy) {
                try {
                    ErlyBerly.nodeAPI().startTrace(function, maxTraceQueueLengthConfig);
                }
                catch (Exception e) {
                    e.printStackTrace();
                    Platform.runLater(() -> { traces.remove(function); });
                }
            }
        });
    }

    public void addTraceListener(InvalidationListener listener) {
        traces.addListener(listener);
    }

    public void requestModFuncs(RpcCallback<OtpErlangList> rpcCallback) {
        new GetModulesThread(rpcCallback).start();
    }

    public void seqTrace(ModFunc value) {
        try {
            ErlyBerly.nodeAPI().seqTrace(value);
            collectingSeqTraces = true;
        }
        catch (OtpErlangException | IOException e) {
            e.printStackTrace();
        }
    }

    class SeqTraceCollectorThread extends Thread {
        private final RpcCallback<SeqTraceLog> callback;

        public SeqTraceCollectorThread(RpcCallback<SeqTraceLog> aCallback) {
            callback = aCallback;

            setDaemon(true);
            setName("Erlyberly Collect Seq Traces");
        }

        @Override
        public void run() {
            while (true) {
                if(collectingSeqTraces && ErlyBerly.nodeAPI().isConnected()) {
                    try {
                        final List<SeqTraceLog> seqTraceLogs = ErlyBerly.nodeAPI().collectSeqTraceLogs();

                        for (SeqTraceLog seqTraceLog : seqTraceLogs) {
                            Platform.runLater(() -> { callback.callback(seqTraceLog); });
                        }
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }

                try {
                    Thread.sleep(100);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
        }
    }

    class GetModulesThread extends Thread {
        private final RpcCallback<OtpErlangList> rpcCallback;

        public GetModulesThread(RpcCallback<OtpErlangList> anRpcCallback) {
            rpcCallback = anRpcCallback;

            setDaemon(true);
            setName("Erlyberly Get Modules");
        }

        @Override
        public void run() {
            try {
                OtpErlangList functions = ErlyBerly.nodeAPI().requestFunctions();
                Platform.runLater(() -> { rpcCallback.callback(functions); });
            }
            catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    public String moduleFunctionSourceCode(String module, String function, Integer arity) throws IOException, OtpErlangException {
        return ErlyBerly.nodeAPI().moduleFunctionSourceCode(module, function, arity);
    }
    public String moduleFunctionSourceCode(String module) throws IOException, OtpErlangException {
        return ErlyBerly.nodeAPI().moduleFunctionSourceCode(module);
    }

    public String moduleFunctionAbstCode(String module) throws IOException, OtpErlangException {
        String moduleCode = ErlyBerly.nodeAPI().moduleFunctionAbstractCode(module);
        return moduleCode;
    }
    public String moduleFunctionAbstCode(String module, String function, Integer arity) throws IOException, OtpErlangException {
        String moduleCode = ErlyBerly.nodeAPI().moduleFunctionAbstractCode(module, function, arity);
        return moduleCode;
    }

    public void setModuleLoadedCallback(RpcCallback<OtpErlangTuple> moduleLoadedCallback) {
        ErlyBerly.nodeAPI().setModuleLoadedCallback(moduleLoadedCallback);
    }

}
