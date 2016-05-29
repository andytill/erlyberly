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

import erlyberly.node.NodeAPI;
import erlyberly.node.NodeAPI.RpcCallback;
import javafx.application.Platform;
import javafx.beans.InvalidationListener;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.fxml.Initializable;


public class DbgController implements Initializable {

    public final ObservableList<TraceLog> traceLogs = FXCollections.observableArrayList();

    private final ObservableList<ModFunc> traces = FXCollections.observableArrayList();

    private final ObservableList<SeqTraceLog> seqTraces = FXCollections.observableArrayList();

    private volatile boolean collectingSeqTraces;

    @Override
    public void initialize(URL url, ResourceBundle r) {
        ErlyBerly.nodeAPI().setTraceLogCallback((traceLog) -> {
            traceLogs.add(traceLog);
        });
        ErlyBerly.nodeAPI().suspendedProperty().addListener((o, oldv, suspended) -> {
            // an un-suspended is similar to a node coming back online, reapply our known traces
            if(!suspended && ErlyBerly.nodeAPI().isConnected()) {
                reapplyTraces();
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
        if(traces.contains(function))
            onRemoveTracer(null, function);
        else
            traceModFunc(function);
    }

    private void traceModFunc(ModFunc function) {
        try {
            ErlyBerly.nodeAPI().startTrace(function);

            traces.add(function);
        }
        catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    private void onRemoveTracer(ActionEvent e, ModFunc function) {
        try {
            ErlyBerly.nodeAPI().stopTrace(function);

            traces.remove(function);
        }
        catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    public void reapplyTraces() {
        ArrayList<ModFunc> tracesCopy = new ArrayList<ModFunc>(traces);

        for (ModFunc function : tracesCopy) {
            try {
                ErlyBerly.nodeAPI().startTrace(function);
            } catch (Exception e) {
                e.printStackTrace();

                traces.remove(function);
            }
        }
    }

    public void addTraceListener(InvalidationListener listener) {
        traces.addListener(listener);
    }

    public void requestModFuncs(NodeAPI.RpcCallback<OtpErlangList> rpcCallback) {
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
        String moduleCode = ErlyBerly.nodeAPI().moduleFunctionSourceCode(module, function, arity);
        return moduleCode;
    }
    public String moduleFunctionSourceCode(String module) throws IOException, OtpErlangException {
        String moduleCode = ErlyBerly.nodeAPI().moduleFunctionSourceCode(module);
        return moduleCode;
    }

    public String moduleFunctionAbstCode(String module) throws IOException, OtpErlangException {
        String moduleCode = ErlyBerly.nodeAPI().moduleFunctionAbstCode(module);
        return moduleCode;
    }
    public String moduleFunctionAbstCode(String module, String function, Integer arity) throws IOException, OtpErlangException {
        String moduleCode = ErlyBerly.nodeAPI().moduleFunctionAbstCode(module, function, arity);
        return moduleCode;
    }

    public void setModuleLoadedCallback(RpcCallback<OtpErlangTuple> moduleLoadedCallback) {
        ErlyBerly.nodeAPI().setModuleLoadedCallback(moduleLoadedCallback);
    }

}
