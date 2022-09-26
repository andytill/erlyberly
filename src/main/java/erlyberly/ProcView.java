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

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import floatyfield.FloatyFieldView;
import javafx.application.Platform;
import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.beans.binding.BooleanBinding;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.geometry.Orientation;
import javafx.scene.chart.PieChart;
import javafx.scene.control.*;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.util.Callback;
import org.controlsfx.glyphfont.FontAwesome;

import java.net.URL;
import java.text.DecimalFormat;
import java.util.ResourceBundle;

/**
 * Handles UI related tasks and delegates processing to {@link ProcController}.
 */
public class ProcView implements Initializable {

    private final ProcController procController;

    @FXML
    private TableView<ProcInfo> processView;
    @FXML
    private Button refreshButton;
    @FXML
    private Button pollButton;
    @FXML
    private Button heapPieButton;
    @FXML
    private Button stackPieButton;
    @FXML
    private Button totalHeapPieButton;
    @FXML
    private HBox headerBox;

    /**
     * Total of the last opened pie chart
     */
    private double total;

    /**
     * Only use on the javafx thread
     */
    private final DecimalFormat percentFormatter = new DecimalFormat("#.#");


    public ProcView() {
        super();
        this.procController = new ProcController();
    }

    @Override
    public void initialize(final URL url, final ResourceBundle r) {
        final MenuItem menuItem;

        menuItem = new MenuItem("Get process state (R16B03 or higher)");
        menuItem.setOnAction(this::onShowProcessStateClicked);

        menuItem.disableProperty().bind(this.processView.getSelectionModel().selectedItemProperty().isNull());

        this.processView.setContextMenu(new ContextMenu(menuItem));

        // #23 when the context menu is showing, temporarily suspend polling, polling
        // loses selection making the right click context menu no longer enabled since
        // no process is selected
        this.processView.getContextMenu().showingProperty().addListener((o, oldv, newv) -> this.procController.setTemporarilySuspendPolling(newv.booleanValue()));

        final BooleanBinding notConnected = ErlyBerly.nodeAPI().connectedProperty().not();

        ErlyBerly.nodeAPI().connectedProperty().addListener(this::onConnected);

        this.heapPieButton.setGraphic(new FontAwesome().create(FontAwesome.Glyph.PIE_CHART));
        this.heapPieButton.getStyleClass().add("erlyberly-icon-button");
        this.heapPieButton.setStyle("-fx-background-color: transparent;");
        this.heapPieButton.setText("");
        this.heapPieButton.disableProperty().bind(notConnected);

        this.stackPieButton.setGraphic(new FontAwesome().create(FontAwesome.Glyph.PIE_CHART));
        this.stackPieButton.setStyle("-fx-background-color: transparent;");
        this.stackPieButton.setText("");
        this.stackPieButton.disableProperty().bind(notConnected);

        this.totalHeapPieButton.setGraphic(new FontAwesome().create(FontAwesome.Glyph.PIE_CHART));
        this.totalHeapPieButton.setStyle("-fx-background-color: transparent;");
        this.totalHeapPieButton.setText("");
        this.totalHeapPieButton.disableProperty().bind(notConnected);

        this.refreshButton.setGraphic(new FontAwesome().create(FontAwesome.Glyph.ROTATE_LEFT));
        this.refreshButton.setGraphicTextGap(8d);
        this.refreshButton.disableProperty().bind(this.procController.pollingProperty().or(notConnected));

        this.pollButton.setGraphic(new FontAwesome().create(FontAwesome.Glyph.REFRESH));
        this.pollButton.setGraphicTextGap(9d);
        this.pollButton.disableProperty().bind(notConnected);

        this.procController.pollingProperty().addListener(this::onPollingChange);
        this.procController.setListComparator(this.processView.comparatorProperty());

        this.onPollingChange(null);

        final TableColumn<ProcInfo, String> pidColumn = (TableColumn<ProcInfo, String>) this.processView.getColumns().get(0);
        final TableColumn<ProcInfo, String> procColumn = (TableColumn<ProcInfo, String>) this.processView.getColumns().get(1);
        final TableColumn<ProcInfo, Long> reducColumn = (TableColumn<ProcInfo, Long>) this.processView.getColumns().get(2);
        final TableColumn<ProcInfo, Long> mQueueLenColumn = (TableColumn<ProcInfo, Long>) this.processView.getColumns().get(3);
        final TableColumn<ProcInfo, Long> heapSizeColumn = (TableColumn<ProcInfo, Long>) this.processView.getColumns().get(4);
        final TableColumn<ProcInfo, Long> stackSizeColumn = (TableColumn<ProcInfo, Long>) this.processView.getColumns().get(5);
        final TableColumn<ProcInfo, Long> totalHeapSizeColumn = (TableColumn<ProcInfo, Long>) this.processView.getColumns().get(6);

        pidColumn.setCellValueFactory(new PropertyValueFactory<>("pid"));
        pidColumn.setId("pid");

        procColumn.setCellValueFactory(new PropertyValueFactory<>("processName"));
        procColumn.setId("proc");

        reducColumn.setCellValueFactory(new PropertyValueFactory<>("reductions"));
        reducColumn.setId("reduc");

        mQueueLenColumn.setCellValueFactory(new PropertyValueFactory<>("msgQueueLen"));
        mQueueLenColumn.setId("mqueue");

        heapSizeColumn.setCellValueFactory(new PropertyValueFactory<>("heapSize"));
        heapSizeColumn.setId("heapsize");

        stackSizeColumn.setCellValueFactory(new PropertyValueFactory<>("stackSize"));
        stackSizeColumn.setId("stacksize");

        totalHeapSizeColumn.setCellValueFactory(new PropertyValueFactory<>("totalHeapSize"));
        totalHeapSizeColumn.setId("totalheapsize");

        this.processView.setItems(this.procController.getProcs());
        this.processView.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);

        this.addFloatySearchControl();

        this.initialiseProcessSorting();
    }

    private void addFloatySearchControl() {
        final FxmlLoadable loader = new FxmlLoadable("/floatyfield/floaty-field.fxml");

        loader.load();

        final FloatyFieldView ffView;

        ffView = (FloatyFieldView) loader.controller;
        ffView.promptTextProperty().set("Filter on process pid and registered name");

        HBox.setHgrow(loader.fxmlNode, Priority.ALWAYS);

        this.headerBox.getChildren().add(0, new Separator(Orientation.VERTICAL));
        this.headerBox.getChildren().add(0, loader.fxmlNode);

        this.procController.filterProperty().bind(ffView.textProperty());

        Platform.runLater(() -> FilterFocusManager.addFilter((Control) loader.fxmlNode.getChildrenUnmodifiable().get(1), 0));

    }

    private void onShowProcessStateClicked(final ActionEvent e) {
        final ProcInfo proc = this.processView.getSelectionModel().getSelectedItem();

        if (null == proc) return;

        ProcController.processState(proc, (eobj) -> ProcView.showProcessStateInWindow(proc, eobj));
    }

    private static void showProcessStateInWindow(final ProcInfo procInfo, final OtpErlangObject obj) {
        OtpErlangObject obj1 = obj;
        if (null == obj1)
            obj1 = new OtpErlangString("Error, erlyberly cannot get process state. Probably not OTP compliant process");

        final TermTreeView termTreeView;

        termTreeView = new TermTreeView();
        termTreeView.setMaxHeight(Integer.MAX_VALUE);
        VBox.setVgrow(termTreeView, Priority.ALWAYS);
        termTreeView.populateFromTerm(obj1);

        ErlyBerly.showPane("Process State for " + procInfo.getShortName(), ErlyBerly.wrapInPane(termTreeView));
    }

    @FXML
    private void onHeapPie() {
        final ObservableList<PieChart.Data> data = this.buildData(this.chartableProcs(), ProcInfo::getHeapSize);

        this.showPieChart("Process Heap", data);
    }

    @FXML
    private void onStackPie() {
        final ObservableList<PieChart.Data> data = this.buildData(this.chartableProcs(), ProcInfo::getStackSize);

        this.showPieChart("Process Stack", data);
    }

    @FXML
    private void onTotalHeapPie() {
        final ObservableList<PieChart.Data> data = this.buildData(this.chartableProcs(), ProcInfo::getTotalHeapSize);

        this.showPieChart("Total Heap", data);
    }

    private ObservableList<PieChart.Data> buildData(final ObservableList<ProcInfo> procs, final Callback<ProcInfo, Long> extractor) {

        this.total = 0;

        for (final ProcInfo proc : procs) {
            this.total += extractor.call(proc).doubleValue();
        }

        // threshold is 0.5%, this is a limit on how many segments are added to
        // the pie chart too many seems to crash the process
        final double threshold = this.total / 200;

        double other = 0;

        final ObservableList<PieChart.Data> data = FXCollections.observableArrayList();

        for (final ProcInfo proc : procs) {
            final double value = extractor.call(proc).doubleValue();

            if (value >= threshold)
                data.add(new PieChart.Data(ProcView.procDescription(proc), extractor.call(proc).doubleValue()));
            else other += value;
        }

        if (0 < other) data.add(new PieChart.Data("All processes less than 0.5% of total", other));

        return data;
    }

    private ObservableList<ProcInfo> chartableProcs() {
        ObservableList<ProcInfo> procs = this.processView.getSelectionModel().getSelectedItems();

        if (procs.isEmpty() || 1 == procs.size()) {
            procs = this.procController.getProcs();
        }
        return procs;
    }

    private void showPieChart(final String title, final ObservableList<PieChart.Data> data) {
        final PieChart pieChart;
        pieChart = new PieChart(data);
        pieChart.setTitle(title);
        pieChart.getData().forEach(d -> {
            final Tooltip tooltip;
            tooltip = new Tooltip();
            final String percent = this.percentFormatter.format((d.getPieValue() / this.total) * 100);
            tooltip.setText(d.getName() + " " + percent + "%");
            Tooltip.install(d.getNode(), tooltip);
        });
        ErlyBerly.showPane(title, ErlyBerly.wrapInPane(pieChart));
    }

    private static String procDescription(final ProcInfo proc) {
        String pid = proc.getProcessName();
        if (null == pid || pid.isEmpty()) {
            pid = proc.getPid();
        }
        if (null == pid || pid.isEmpty()) {
            pid = "unknown pid";
        }
        return pid;
    }

    private void onPollingChange(final Observable o) {
        if (this.procController.pollingProperty().get()) this.pollButton.setText("Stop Polling");
        else this.pollButton.setText("Start Polling");
    }

    @FXML
    private void onRefresh() {
        this.procController.refreshOnce();
    }

    @FXML
    private void onTogglePolling() {
        this.procController.togglePolling();
    }

    private void onConnected(final Observable o) {

        final boolean connected = ErlyBerly.nodeAPI().connectedProperty().get();

        if (connected) {
            this.procController.refreshOnce();
        } else {
            this.procController.clearProcesses();
        }
    }

    private void initialiseProcessSorting() {
        final InvalidationListener invalidationListener = new ProcSortUpdater();

        for (final TableColumn<ProcInfo, ?> col : this.processView.getColumns()) {
            col.sortTypeProperty().addListener(invalidationListener);
        }

        this.processView.getSortOrder().addListener(invalidationListener);
    }

    private final class ProcSortUpdater implements InvalidationListener {
        @Override
        public void invalidated(final Observable ob) {
            ProcSort procSort = null;

            if (!ProcView.this.processView.getSortOrder().isEmpty()) {
                final TableColumn<ProcInfo, ?> tableColumn = ProcView.this.processView.getSortOrder().get(0);

                procSort = new ProcSort(tableColumn.getId(), tableColumn.getSortType());
            }
            ProcView.this.procController.procSortProperty().set(procSort);
        }
    }
}
