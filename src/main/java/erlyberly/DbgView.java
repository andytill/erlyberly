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

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import floatyfield.FloatyFieldView;
import javafx.application.Platform;
import javafx.beans.Observable;
import javafx.beans.binding.Bindings;
import javafx.beans.property.StringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.collections.transformation.SortedList;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.*;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.Glyph;
import ui.TabPaneDetacher;

import java.net.URL;
import java.util.*;
import java.util.concurrent.ScheduledFuture;


public class DbgView implements Initializable {

    private static final String MODULES_TREE_PREF_WIDTH_CONFIG_KEY = "modulesTreePrefWidth";

    private static final double MODULES_TREE_PREF_WIDTH_CONFIG_KEY_DEFAULT = 300;

    private static final String ICON_STYLE = "-fx-font-family: FontAwesome; -fx-font-size: 1em;";

    private final DbgController dbgController = new DbgController();

    private final ObservableList<TreeItem<ModFunc>> treeModules = FXCollections.observableArrayList();

    private final SortedList<TreeItem<ModFunc>> sortedTreeModules = new SortedList<>(this.treeModules);

    private final FilteredList<TreeItem<ModFunc>> filteredTreeModules = new FilteredList<>(this.sortedTreeModules);

    /**
     * A list of all the filtered lists for functions, so a predicate can be set on them.  Binding
     * the predicate property does not seem to work.
     */
    private final Map<ModFunc, FilteredList<TreeItem<ModFunc>>> functionLists = new HashMap<>();

    @FXML
    private TreeView<ModFunc> modulesTree;
    @FXML
    private VBox modulesBox;
    @FXML
    private Label noTracesLabel;
    @FXML
    private SplitPane dbgSplitPane;
    @FXML
    private HBox traceLogSearchBox;

    private double functionsDivPosition;

    private ModFuncContextMenu modFuncContextMenu;

    @Override
    public void initialize(final URL url, final ResourceBundle r) {
        this.modFuncContextMenu = new ModFuncContextMenu(this.dbgController);
        this.modFuncContextMenu.setModuleTraceMenuText("Trace All Functions in Module");
        this.modFuncContextMenu.rootProperty().bind(this.modulesTree.rootProperty());
        this.modulesTree.getSelectionModel().selectedItemProperty().addListener((o, old, newItem) -> {
            this.modFuncContextMenu.selectedTreeItemProperty().set(newItem);
            if (null != newItem) this.modFuncContextMenu.selectedItemProperty().set(newItem.getValue());
        });

        this.sortedTreeModules.setComparator(DbgView.treeItemModFuncComparator());

        SplitPane.setResizableWithParent(this.modulesBox, Boolean.TRUE);

        ErlyBerly.nodeAPI().connectedProperty().addListener(this::onConnected);

        this.modulesTree.setCellFactory(new ModFuncTreeCellFactory(this.dbgController));
        this.modulesTree.setContextMenu(this.modFuncContextMenu);

        this.addModulesFloatySearchControl();

        this.dbgController.initialize(url, r);
        DbgController.setModuleLoadedCallback(this::createModuleTreeItem);
        this.tabPane = new TabPane();
        TabPaneDetacher.create().stylesheets("/floatyfield/floaty-field.css", "/erlyberly/erlyberly.css").makeTabsDetachable(this.tabPane);
        final Tab traceViewTab;
        traceViewTab = new Tab("Traces");
        traceViewTab.setContent(new DbgTraceView(this.dbgController));
        traceViewTab.setClosable(false);
        this.tabPane.getTabs().add(traceViewTab);
        this.dbgSplitPane.getItems().add(this.tabPane);
    }

    private void addModulesFloatySearchControl() {
        final FxmlLoadable loader = new FxmlLoadable("/floatyfield/floaty-field.fxml");

        loader.load();

        final FloatyFieldView ffView;

        ffView = (FloatyFieldView) loader.controller;
        ffView.promptTextProperty().set("Filter functions i.e. gen_s:call or #t for all traces");

        loader.fxmlNode.setStyle("-fx-padding: 5 5 0 5;");

        HBox.setHgrow(loader.fxmlNode, Priority.ALWAYS);
        this.modulesBox.getChildren().add(0, loader.fxmlNode);

        this.filterTextProperty = ffView.textProperty();
        this.filterTextProperty.addListener(this::onFunctionSearchChange);

        final TextField filterTextView;
        filterTextView = DbgView.floatyFieldTextField(loader);

        Platform.runLater(() -> FilterFocusManager.addFilter(filterTextView, 1));

    }

    /**
     * Flag for when the shortcut for applying traces to all visisble functions
     * is pressed down, used for only executing it once per press, not once per
     * event which can be many.
     */
    static boolean toggleAllTracesDown;

    private StringProperty filterTextProperty;

    private TabPane tabPane;

    /**
     * A scheduled future that gets created when the module filter text is
     * changed, when it is changed again it is cancelled and a new one is
     * created. If no change occurs within the delay time then the module in
     * the filter will be loaded.
     */
    private ScheduledFuture<?> scheduledModuleLoadFuture;

    private static TextField floatyFieldTextField(final FxmlLoadable loader) {
        // FIXME floaty field should allow access to the text field
        return (TextField) loader.fxmlNode.getChildrenUnmodifiable().get(1);
    }


    public void onRefreshModules(final ActionEvent e) {
        this.treeModules.clear();

        this.refreshModules();
    }

    public void onFunctionSearchChange(final Observable o, final String oldValue, final String search) {
        if (DbgView.isSpecialTraceFilter(search)) this.filterForTracedFunctions();
        else this.filterForFunctionTextMatch(search);
    }

    private static boolean isSpecialTraceFilter(final String search) {
        return "#t".equals(search.trim());
    }

    private void filterForFunctionTextMatch(final String search) {
        final String[] split = search.split(":");

        if (0 == split.length) return;

        final String moduleName = split[0];
        final String funcName = (1 < split.length) ? split[1] : "";

        if (search.contains(":")) {
            for (final TreeItem<ModFunc> treeItem : this.filteredTreeModules) {
                treeItem.setExpanded(true);
            }
        }

        for (final FilteredList<TreeItem<ModFunc>> funcItemList : this.functionLists.values()) {
            funcItemList.setPredicate((t) -> DbgView.isMatchingModFunc(funcName, t));
        }

        this.filteredTreeModules.setPredicate((t) -> DbgView.isMatchingModFunc(moduleName, t) && !t.getChildren().isEmpty());
    }

    private void filterForTracedFunctions() {
        for (final FilteredList<TreeItem<ModFunc>> funcItemList : this.functionLists.values()) {
            funcItemList.setPredicate((t) -> this.dbgController.isTraced(t.getValue()));
        }

        this.filteredTreeModules.setPredicate((t) -> !t.getChildren().isEmpty());
    }

    private static Comparator<TreeItem<ModFunc>> treeItemModFuncComparator() {
        return Comparator.comparing(TreeItem::getValue);
    }

    private static boolean isMatchingModFunc(final String searchText, final TreeItem<ModFunc> t) {
        if (searchText.isEmpty()) return true;
        return t.getValue().toString().contains(searchText);
    }

    private void onConnected(final Observable o) {
        final boolean connected = ErlyBerly.nodeAPI().connectedProperty().get();

        // disable buttons when not connected
        /*seqTraceMenuItem.setDisable(!connected);*/

        if (connected) {
            this.refreshModules();
            this.dbgController.reapplyTraces();
        } else {
            // Don't clear the Traces, keep it, an re-apply once connected again.
            this.treeModules.clear();
        }
    }

    private void refreshModules() {
        try {
            this.modulesTree.setShowRoot(false);
            DbgController.requestModFuncs(this::buildObjectTreeRoot);
        } catch (final Exception e) {
            throw new RuntimeException("failed to build module/function tree", e);
        }
    }

    private void buildObjectTreeRoot(final OtpErlangList requestFunctions) {
        for (final OtpErlangObject e : requestFunctions) {
            final OtpErlangTuple tuple = (OtpErlangTuple) e;

            this.createModuleTreeItem(tuple);
        }

        final TreeItem<ModFunc> root;

        root = new TreeItem<>();
        root.setExpanded(true);

        Bindings.bindContentBidirectional(root.getChildren(), this.filteredTreeModules);

        this.modulesTree.setRoot(root);

        // set predicates on the function tree items so that they filter correctly
        this.filterForFunctionTextMatch(this.filterTextProperty.get());

        // this listener tries to load a module typed into the filter
        // so that the user does not have to load it into the vm first
        // downside is that it dynamically creates atoms at a max rate
        // of one per second, not a huge amount
        //
        // TODO if there is a "production" mode then this feature should
        //      disabled, alternatively we can check if the application is
        //      embedded, if it is then all modules should be loaded anyway
        this.filterTextProperty.addListener((o, oldString, newString) -> {
            if (null != this.scheduledModuleLoadFuture) {
                this.scheduledModuleLoadFuture.cancel(true);
            }
            final String[] split = newString.trim().split(":");
            if (newString.isEmpty() || 0 == split.length) return;
            final String moduleName = split[0];
            if (null == moduleName || moduleName.isEmpty()) {
                return;
            }
            final long delayMillis = 1000;
            this.scheduledModuleLoadFuture = ErlyBerly.scheduledIO(delayMillis, () -> {
                try {
                    ErlyBerly.nodeAPI().tryLoadModule(moduleName);
                } catch (final Exception e1) {
                    e1.printStackTrace();
                }
            });
        });
    }

    private void createModuleTreeItem(final OtpErlangTuple tuple) {
        final boolean isExported;
        final OtpErlangAtom moduleNameAtom = (OtpErlangAtom) tuple.elementAt(0);
        final OtpErlangList exportedFuncs = (OtpErlangList) tuple.elementAt(1);
        final OtpErlangList localFuncs = (OtpErlangList) tuple.elementAt(2);

        final TreeItem<ModFunc> moduleItem;

        final ModFunc module = ModFunc.toModule(moduleNameAtom);
        moduleItem = new TreeItem<>(module);
        moduleItem.setGraphic(DbgView.treeIcon(FontAwesome.Glyph.CUBE));

        final ObservableList<TreeItem<ModFunc>> modFuncs = FXCollections.observableArrayList();

        final SortedList<TreeItem<ModFunc>> sortedFuncs = new SortedList<>(modFuncs);

        final FilteredList<TreeItem<ModFunc>> filteredFuncs = new FilteredList<>(sortedFuncs);

        sortedFuncs.setComparator(DbgView.treeItemModFuncComparator());

        DbgView.addTreeItems(DbgView.toModFuncs(moduleNameAtom, exportedFuncs, true), modFuncs);

        DbgView.addTreeItems(DbgView.toModFuncs(moduleNameAtom, localFuncs, false), modFuncs);
        this.functionLists.put(module, filteredFuncs);

        Bindings.bindContentBidirectional(moduleItem.getChildren(), filteredFuncs);

        final Iterable<TreeItem<ModFunc>> treeModulesCopy = new ArrayList<>(this.treeModules);
        for (final TreeItem<ModFunc> treeItem : treeModulesCopy) {
            if (treeItem.getValue().equals(module)) {
                this.treeModules.remove(treeItem);
            }
        }
        this.treeModules.add(moduleItem);
    }

    private static void addTreeItems(final List<ModFunc> modFuncs, final ObservableList<TreeItem<ModFunc>> modFuncTreeItems) {
        for (final ModFunc modFunc : modFuncs) {
            if (!modFunc.isSynthetic()) {
                final TreeItem<ModFunc> item = DbgView.newFuncTreeItem(modFunc);

                modFuncTreeItems.add(item);
            }
        }
    }

    private static TreeItem<ModFunc> newFuncTreeItem(final ModFunc modFunc) {
        return new TreeItem<>(modFunc);
    }


    private static Glyph treeIcon(final FontAwesome.Glyph treeIcon) {
        final Glyph icon = new FontAwesome().create(treeIcon);
        icon.setStyle(ICON_STYLE);
        return icon;
    }

    private static List<ModFunc> toModFuncs(final OtpErlangAtom moduleNameAtom, final OtpErlangList exportedFuncs, final boolean isExported) {
        final List<ModFunc> mfs = new ArrayList<>();
        for (final OtpErlangObject exported : exportedFuncs) {
            final ModFunc modFunc = ModFunc.toFunc(moduleNameAtom, exported, isExported);
            mfs.add(modFunc);
        }
        return mfs;
    }

    public void setFunctionsVisibility(final Boolean hidden) {
        if (hidden.booleanValue()) {
            final SplitPane.Divider div = this.dbgSplitPane.getDividers().get(0);

            this.functionsDivPosition = div.getPosition();

            div.setPosition(0d);
            this.dbgSplitPane.getItems().remove(0);
        } else {
            this.dbgSplitPane.getItems().add(0, this.modulesBox);

            final SplitPane.Divider div = this.dbgSplitPane.getDividers().get(0);
            div.setPosition(this.functionsDivPosition);
        }
    }

    public TabPane getTabPane() {
        return this.tabPane;
    }

    public void sizeSplitPanes() {
        assert null != this.dbgSplitPane.getScene();
        assert 0.0d < this.dbgSplitPane.getScene().getWidth();
        try {
            final double percent = (DbgView.configuredModulesWidth() / this.dbgSplitPane.getScene().getWidth());
            // the split pane divider position can only be set as a percentage of the split pane
            this.dbgSplitPane.setDividerPosition(0, percent);
        } catch (final NumberFormatException e) {
            e.printStackTrace();
        }
        // whenever the width of the pane changes, write it to configuration
        // this is buffered so rapid writes do not cause rapid writes to disk
        this.modulesBox.widthProperty().addListener((o, ov, nv) -> PrefBind.set(MODULES_TREE_PREF_WIDTH_CONFIG_KEY, nv));
    }

    private static double configuredModulesWidth() {
        return PrefBind.getOrDefaultDouble(MODULES_TREE_PREF_WIDTH_CONFIG_KEY, MODULES_TREE_PREF_WIDTH_CONFIG_KEY_DEFAULT);
    }
}
