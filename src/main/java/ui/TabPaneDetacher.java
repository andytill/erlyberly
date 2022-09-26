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
package ui;

/*
 * Copyright 2014 Jens Deters.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

import javafx.beans.Observable;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.scene.Cursor;
import javafx.scene.Scene;
import javafx.scene.SnapshotParameters;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.image.WritableImage;
import javafx.scene.input.ClipboardContent;
import javafx.scene.input.DataFormat;
import javafx.scene.input.DragEvent;
import javafx.scene.input.Dragboard;
import javafx.scene.input.MouseEvent;
import javafx.scene.input.TransferMode;
import javafx.scene.layout.Pane;
import javafx.scene.transform.Transform;
import javafx.stage.Stage;
import javafx.stage.WindowEvent;

/**
 * A simple Utility to make all {@link Tab}s of a {@link TabPane} detachable.
 * <br>
 * <h1>Usage</h1>
 * To get a {@link TabPane} in charge of control:
 * <br>
 * <b>Hint: Only already added {@link Tab}s are going to be in charge of control!</b>
 * <pre>
 * {@code
 * TabPaneDude.create().makeTabsDetachable(myTapPane);
 * }
 * </pre> Tabs can then be detached simply by dragging a tab title to the desired window position.
 *
 /**
 *
 * @author Jens Deters (www.jensd.de)
 * @version 1.0.0
 * @since 14-10-2014
 */
public class TabPaneDetacher {

    private TabPane tabPane;
    private Tab currentTab;
    private final List<Tab> originalTabs;
    private final Map<Integer, Tab> tapTransferMap;
    private String[] stylesheets;
    private final BooleanProperty alwaysOnTop;

    private TabPaneDetacher() {
        originalTabs = new ArrayList<>();
        stylesheets = new String[]{};
        tapTransferMap = new HashMap<>();
        alwaysOnTop = new SimpleBooleanProperty();
    }

    /**
     * Creates a new instance of the TabPaneDetacher
     *
     * @return The new instance of the TabPaneDetacher.
     */
    public static TabPaneDetacher create() {
        return new TabPaneDetacher();
    }

    public BooleanProperty alwaysOnTopProperty() {
        return alwaysOnTop;
    }

    public Boolean isAlwaysOnTop() {
        return alwaysOnTop.get();
    }

    /**
     *
     * Sets whether detached Tabs should be always on top.
     *
     * @param alwaysOnTop The state to be set.
     * @return The current TabPaneDetacher instance.
     */
    public TabPaneDetacher alwaysOnTop(boolean alwaysOnTop) {
        alwaysOnTopProperty().set(alwaysOnTop);
        return this;
    }

    /**
     * Sets the stylesheets that should be assigend to the new created {@link Stage}.
     *
     * @param stylesheets The stylesheets to be set.
     * @return The current TabPaneDetacher instance.
     */
    public TabPaneDetacher stylesheets(String... stylesheets) {
        this.stylesheets = stylesheets;
        return this;
    }

    /**
     * Make all added {@link Tab}s of the given {@link TabPane} detachable.
     *
     * @param tabPane The {@link TabPane} to take over.
     * @return The current TabPaneDetacher instance.
     */
    public TabPaneDetacher makeTabsDetachable(TabPane tabPane) {
        this.tabPane = tabPane;
        originalTabs.addAll(tabPane.getTabs());
        for (int i = 0; i < tabPane.getTabs().size(); i++) {
            tapTransferMap.put(i, tabPane.getTabs().get(i));
        }
        tabPane.getTabs().stream().forEach(t -> {
            t.setClosable(false);
        });
        tabPane.getTabs().addListener((Observable o) -> {
            for (Tab tabX : tabPane.getTabs()) {
                if (!(tabX.getContent() instanceof Pane)) {
                    throw new RuntimeException("Tab added where the content node was not a subclass of Pane, this means it cannot be dragged by TabPaneDetacher.");
                }
            }
        });
        tabPane.setOnDragDetected(
                (MouseEvent event) -> {
                    if (event.getSource() instanceof TabPane) {
                        Pane rootPane = (Pane) tabPane.getScene().getRoot();
                        rootPane.setOnDragOver((DragEvent event1) -> {
                            event1.acceptTransferModes(TransferMode.ANY);
                            event1.consume();
                        });
                        currentTab = tabPane.getSelectionModel().getSelectedItem();
                        SnapshotParameters snapshotParams = new SnapshotParameters();
                        snapshotParams.setTransform(Transform.scale(0.4, 0.4));
                        WritableImage snapshot = currentTab.getContent().snapshot(snapshotParams, null);
                        Dragboard db = tabPane.startDragAndDrop(TransferMode.MOVE);
                        ClipboardContent clipboardContent = new ClipboardContent();
                        clipboardContent.put(DataFormat.PLAIN_TEXT, "detach");
                        db.setDragView(snapshot, 40, 40);
                        db.setContent(clipboardContent);
                    }
                    event.consume();
                }
        );
        tabPane.setOnDragDone(
                (DragEvent event) -> {
                    openTabInStage(currentTab);
                    tabPane.setCursor(Cursor.DEFAULT);
                    event.consume();
                }
        );
        return this;
    }

    /**
     * Opens the content of the given {@link Tab} in a separate Stage. While the content is removed from the {@link Tab} it is
     * added to the root of a new {@link Stage}. The Window title is set to the name of the {@link Tab};
     *
     * @param tab The {@link Tab} to get the content from.
     */
    public void openTabInStage(final Tab tab) {
        if (tab == null) {
            return;
        }
        int originalTab = originalTabs.indexOf(tab);
        tapTransferMap.remove(originalTab);
        Pane content = (Pane) tab.getContent();
        if (content == null) {
            throw new IllegalArgumentException("Can not detach Tab '" + tab.getText() + "': content is empty (null).");
        }
        tab.setContent(null);
        final Scene scene = new Scene(content, content.getPrefWidth(), content.getPrefHeight());
        scene.getStylesheets().addAll(stylesheets);
        Stage stage = new Stage();
        stage.setScene(scene);
        stage.setTitle(tab.getText());
        stage.setAlwaysOnTop(isAlwaysOnTop());
        stage.setOnCloseRequest((WindowEvent t) -> {
            stage.close();
            tab.setContent(content);
            int originalTabIndex = originalTabs.indexOf(tab);
            tapTransferMap.put(originalTabIndex, tab);
            int index = 0;
            SortedSet<Integer> keys = new TreeSet<>(tapTransferMap.keySet());
            for (Integer key : keys) {
                Tab value = tapTransferMap.get(key);
                if (!tabPane.getTabs().contains(value)) {
                    tabPane.getTabs().add(index, value);
                }
                index++;
            }
            tabPane.getSelectionModel().select(tab);
        });
        stage.setOnShown((WindowEvent t) -> {
            tab.getTabPane().getTabs().remove(tab);
        });
        CloseWindowOnEscape.apply(scene, stage);
        stage.show();
    }

}
