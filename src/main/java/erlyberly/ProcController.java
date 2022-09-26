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
import com.ericsson.otp.erlang.OtpErlangObject;
import erlyberly.node.RpcCallback;
import javafx.application.Platform;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.collections.transformation.SortedList;
import javafx.scene.control.TableColumn;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/**
 * Logic and processing for the entop control.
 */
public class ProcController {

    private final SimpleBooleanProperty polling;

    private final SimpleObjectProperty<ProcSort> procSortProperty;

    private final ProcPollerThread procPollerThread;

    private final Object waiter;

    private int timeout = -1;

    private final SimpleStringProperty filter = new SimpleStringProperty();

    private final ObservableList<ProcInfo> processes = FXCollections.observableArrayList();

    private final FilteredList<ProcInfo> filteredProcesses = new FilteredList<>(this.processes);

    private final SortedList<ProcInfo> sortedProcesses = new SortedList<>(this.filteredProcesses);

    private volatile boolean temporarilySuspendPolling;

    public ProcController() {
        super();
        this.polling = new SimpleBooleanProperty();

        this.procSortProperty = new SimpleObjectProperty<>(new ProcSort("reduc", TableColumn.SortType.DESCENDING));

        this.procPollerThread = new ProcPollerThread();

        this.waiter = new Object();

        Platform.runLater(() -> ErlyBerly.nodeAPI().connectedProperty().addListener((o) -> this.startPollingThread()));

        this.filter.addListener((o, ov, nv) -> this.updateProcFilter(nv));
    }

    public void setListComparator(final ObservableValue<? extends Comparator<? super ProcInfo>> tableComparator) {
        this.sortedProcesses.comparatorProperty().bind(tableComparator);
    }

    private void updateProcFilter(final String filterText) {
        final BasicSearch basicSearch = new BasicSearch(filterText);
        this.filteredProcesses.setPredicate((proc) -> ProcController.isMatchingProcess(basicSearch, proc));
    }

    private static boolean isMatchingProcess(final BasicSearch basicSearch, final ProcInfo proc) {
        return basicSearch.matches(proc.getPid(), proc.getProcessName());
    }

    private void startPollingThread() {
        if (ErlyBerly.nodeAPI().connectedProperty().get() && !this.procPollerThread.isAlive()) {
            this.procPollerThread.start();
        }
    }

    public SimpleStringProperty filterProperty() {
        return this.filter;
    }

    public void refreshOnce() {
        synchronized (this.waiter) {
            this.waiter.notifyAll();
        }
    }

    public void togglePolling() {
        if (-1 == this.timeout) {
            this.timeout = 1000;

            this.refreshOnce();
        } else {
            this.timeout = -1;
        }
        this.polling.set(0 < this.timeout);
    }

    public void clearProcesses() {
        this.processes.clear();
    }

    public ObservableList<ProcInfo> getProcs() {
        return this.sortedProcesses;
    }

    public SimpleBooleanProperty pollingProperty() {
        return this.polling;
    }

    public SimpleObjectProperty<ProcSort> procSortProperty() {
        return this.procSortProperty;
    }

    public static SimpleBooleanProperty connectedProperty() {
        return ErlyBerly.nodeAPI().connectedProperty();
    }

    public boolean isTemporarilySuspendPolling() {
        return this.temporarilySuspendPolling;
    }

    public void setTemporarilySuspendPolling(final boolean temporarilySuspendPolling) {
        this.temporarilySuspendPolling = temporarilySuspendPolling;
    }

    private final class ProcPollerThread extends Thread {
        private ProcPollerThread() {
            super();
            // make sure we don't hang the VM on close because of this thread
            this.setDaemon(true);
            this.setName("Process Info Poller");
        }

        @Override
        public void run() {

            while (true) {

                final boolean connected = ErlyBerly.nodeAPI().isConnected();
                if (!connected) continue;

                final List<ProcInfo> processList = new ArrayList<>();

                if (!ProcController.this.temporarilySuspendPolling) {
                    try {
                        ErlyBerly.nodeAPI().retrieveProcessInfo(processList);

                        this.updateProcessList(processList);
                    } catch (final Exception e) {
                        e.printStackTrace();
                    }
                }

                try {
                    synchronized (ProcController.this.waiter) {
                        if (0 < ProcController.this.timeout)
                            ProcController.this.waiter.wait(ProcController.this.timeout);
                        else ProcController.this.waiter.wait();
                    }
                } catch (final InterruptedException e1) {
                    e1.printStackTrace();
                }
            }
        }

        private void updateProcessList(final List<ProcInfo> processList) {
            Platform.runLater(() -> {
                final ProcSort procSort = ProcController.this.procSortProperty.get();
                if (null != procSort) {
                    Comparator<ProcInfo> comparator = null;

                    if ("proc".equals(procSort.getSortField())) {
                        comparator = Comparator.comparing(ProcInfo::getProcessName);
                    } else if ("reduc".equals(procSort.getSortField())) {
                        comparator = Comparator.comparingLong(ProcInfo::getReductions);
                    }

                    if (null != comparator) {
                        if (TableColumn.SortType.DESCENDING == procSort.getSortType()) {
                            comparator = Collections.reverseOrder(comparator);
                        }
                        processList.sort(comparator);
                    }
                }
                ProcController.this.processes.clear();
                ProcController.this.processes.addAll(processList);
            });
        }
    }

    public static void processState(final ProcInfo proc, final RpcCallback<OtpErlangObject> callback) {
        new ProcessStateThread(proc.getPid(), callback).start();
    }

    static class ProcessStateThread extends Thread {

        private final String pidString;
        private final RpcCallback<OtpErlangObject> callback;

        ProcessStateThread(final String aPidString, final RpcCallback<OtpErlangObject> aCallback) {
            super();
            this.pidString = aPidString;
            this.callback = aCallback;

            this.setDaemon(true);
            this.setName("Erlyberly Get Process State");
        }

        @Override
        public void run() {
            try {
                final OtpErlangObject processState = ErlyBerly.nodeAPI().getProcessState(this.pidString);

                Platform.runLater(() -> this.callback.callback(processState));
            } catch (final OtpErlangException | IOException e) {
                e.printStackTrace();
            }
        }
    }
}
