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

import com.ericsson.otp.erlang.*;
import erlyberly.format.TermFormatter;
import erlyberly.node.OtpUtil;
import erlyberly.node.RecordManager;
import hextstar.HexstarView;
import javafx.event.ActionEvent;
import javafx.scene.control.*;
import javafx.scene.input.Clipboard;
import javafx.scene.input.ClipboardContent;
import javafx.scene.input.KeyCombination;

import java.io.IOException;
import java.util.List;
import java.util.Map;

@SuppressWarnings("rawtypes")
public class TermTreeView extends TreeView<TermTreeItem> {

    private static final OtpErlangAtom DICT_ATOM = OtpUtil.atom("dict");
    private static final OtpErlangAtom ERLYBERLY_RECORD_FIELD_ATOM = OtpUtil.atom("erlyberly_record_field");
    private OtpErlangAtom moduleName;

    public TermTreeView() {
        super();
        this.getStyleClass().add("term-tree");
        this.setRoot(new TreeItem<>());

        final MenuItem copyMenuItem = new MenuItem("Copy");
        copyMenuItem.setAccelerator(KeyCombination.keyCombination("shortcut+c"));
        copyMenuItem.setOnAction(this::onCopyCalls);

        final MenuItem dictMenuItem = new MenuItem("Dict to List");
        dictMenuItem.setOnAction(this::onViewDict);

        final MenuItem hexViewMenuItem = new MenuItem("Hex View");
        hexViewMenuItem.setOnAction(this::onHexView);

        final MenuItem decompileFunContextMenu = new MenuItem("Decompile Fun");
        decompileFunContextMenu.setOnAction(this::onDecompileFun);

        this.setContextMenu(new ContextMenu(copyMenuItem, dictMenuItem, decompileFunContextMenu, hexViewMenuItem));
    }

    public void onHexView(final ActionEvent e) {
        final TreeItem<TermTreeItem> item = this.getSelectionModel().getSelectedItem();
        if (null != item && item.getValue().getObject() instanceof OtpErlangBinary) {
            final OtpErlangBinary binary = (OtpErlangBinary) item.getValue().getObject();
            final HexstarView hexstarView;
            hexstarView = new HexstarView();
            hexstarView.setBinary(binary);
            ErlyBerly.showPane("Hex View", ErlyBerly.wrapInPane(hexstarView));
        }
    }

    public void onDecompileFun(final ActionEvent e) {
        final TreeItem<TermTreeItem> item = this.getSelectionModel().getSelectedItem();
        if (null != item && item.getValue().getObject() instanceof OtpErlangFun) {
            final OtpErlangFun fun = (OtpErlangFun) item.getValue().getObject();
            try {
                final String funSource = ErlyBerly.nodeAPI().decompileFun(fun);
                ErlyBerly.showPane(fun + " Source Code", ErlyBerly.wrapInPane(new CodeView(funSource)));
            } catch (final OtpErlangException e1) {
                e1.printStackTrace();
            } catch (final IOException e1) {
                e1.printStackTrace();
            }
        }
    }

    public void onViewDict(final ActionEvent e) {
        final TreeItem<TermTreeItem> item = this.getSelectionModel().getSelectedItem();
        if (null == item) return;
        if (null == item.getValue() || !DICT_ATOM.equals(item.getValue().getObject())) return;
        if (null == item.getParent() || !(item.getParent().getValue().getObject() instanceof OtpErlangTuple)) return;

        final OtpErlangObject dict = item.getParent().getValue().getObject();

        try {
            final OtpErlangList props = ErlyBerly.nodeAPI().dictToPropslist(dict);
            final TermTreeView parentControl = new TermTreeView();
            parentControl.populateFromTerm(props);
            ErlyBerly.showPane("dict_to_list", ErlyBerly.wrapInPane(parentControl));
        } catch (final Exception e1) {
            e1.printStackTrace();
        }
    }

    public void populateFromListContents(final OtpErlangAtom moduleName, final OtpErlangList list) {
        for (final OtpErlangObject a : list) {
            this.populateFromTerm(moduleName, a);
        }
    }

    public void populateFromListContents(final OtpErlangList list) {
        this.populateFromListContents(null, list);
    }

    public void populateFromTerm(final OtpErlangObject obj) {
        this.populateFromTerm(null, obj);
    }


    public void populateFromTerm(final OtpErlangAtom moduleName, final OtpErlangObject obj) {
        this.moduleName = moduleName;
        this.setShowRoot(false);
        this.addToTreeItem(this.getRoot(), obj);
    }

    private void addToTreeItem(final TreeItem<TermTreeItem> parent, final OtpErlangObject obj) {
        final TermFormatter f = ErlyBerly.getTermFormatter();

        if (obj instanceof OtpErlangBinary) {
            final String termString = f.toString(obj);
            final TreeItem<TermTreeItem> item = new TreeItem<>(new TermTreeItem(obj, termString));
            parent.getChildren().add(item);
        } else if (obj instanceof OtpErlangTuple) {
            OtpErlangObject[] elements = ((OtpErlangTuple) obj).elements();

            if (0 == elements.length) {
                parent.getChildren().add(new TreeItem<>(new TermTreeItem(obj, f.emptyTupleString())));
            } else {
                final TreeItem<TermTreeItem> tupleItem;
                if (OtpUtil.isErlyberlyRecord(obj)) {
                    // this is the "old" way show tuples with record metadata, where the metadata is in the
                    // term itself, rather than stored separately in the RecordManager
                    final String recordNameText = "#" + OtpUtil.tupleElement(1, obj) + " ";

                    tupleItem = new TreeItem<>(new TermTreeItem(obj, f.tupleLeftParen()));
                    tupleItem.setGraphic(TermTreeView.recordLabel(recordNameText));
                    parent.getChildren().add(tupleItem);
                    tupleItem.setExpanded(true);
                    elements = OtpUtil.iterableElements(OtpUtil.tupleElement(2, obj));
                    for (final OtpErlangObject e : elements) {
                        this.addToTreeItem(tupleItem, e);
                    }
                    parent.getChildren().add(new TreeItem<>(new TermTreeItem(obj, f.tupleRightParen())));
                } else if (TermTreeView.isRecordField(obj)) {
                    tupleItem = new TreeItem<>(new TermTreeItem(obj, " "));
                    final String recordField;
                    final OtpErlangObject recordFieldNameObj = OtpUtil.tupleElement(1, obj);
                    // do not toString an OtpErlangString because that will add the quotes around it
                    if (recordFieldNameObj instanceof OtpErlangString) {
                        recordField = ((OtpErlangString) recordFieldNameObj).stringValue();
                    } else {
                        recordField = recordFieldNameObj.toString();
                    }
                    tupleItem.setGraphic(TermTreeView.recordLabel(recordField + " = "));
                    tupleItem.setExpanded(true);

                    parent.getChildren().add(tupleItem);

                    final OtpErlangObject value = OtpUtil.tupleElement(2, obj);
                    if (OtpUtil.isLittleTerm(value)) tupleItem.setValue(new TermTreeItem(value, f.toString(obj)));
                    else this.addToTreeItem(tupleItem, value);
                } else {
                    final List<String> recordNames;
                    final OtpErlangTuple objTuple = (OtpErlangTuple) obj;
                    if (null != (recordNames = this.findRecordDef(objTuple))) {
                        final String recordNameText = "#" + OtpUtil.tupleElement(0, obj) + " ";
                        tupleItem = new TreeItem<>(new TermTreeItem(obj, f.tupleLeftParen()));
                        tupleItem.setGraphic(TermTreeView.recordLabel(recordNameText));
                        parent.getChildren().add(tupleItem);
                        tupleItem.setExpanded(true);
                        elements = objTuple.elements();
                        for (int i = 1; i < elements.length; i++) {
                            this.addToTreeItem(tupleItem, OtpUtil.tuple(ERLYBERLY_RECORD_FIELD_ATOM, recordNames.get(i - 1), elements[i]));
                        }
                        parent.getChildren().add(new TreeItem<>(new TermTreeItem(obj, f.tupleRightParen())));
                    } else {
                        tupleItem = new TreeItem<>();
                        tupleItem.setExpanded(true);
                        if (OtpUtil.isLittleTerm(obj)) {
                            tupleItem.setValue(new TermTreeItem(obj, f.toString(obj)));
                            parent.getChildren().add(tupleItem);
                        } else {
                            tupleItem.setValue(new TermTreeItem(obj, f.tupleLeftParen()));
                            for (final OtpErlangObject e : elements) {
                                this.addToTreeItem(tupleItem, e);
                            }
                            parent.getChildren().add(tupleItem);
                            parent.getChildren().add(new TreeItem<>(new TermTreeItem(obj, f.tupleRightParen())));
                        }
                    }
                }
            }
        } else if (obj instanceof OtpErlangList) {
            final OtpErlangObject[] elements = ((OtpErlangList) obj).elements();

            if (0 == elements.length) {
                parent.getChildren().add(new TreeItem<>(new TermTreeItem(obj, f.emptyListString())));
            } else {
                final TreeItem<TermTreeItem> listItem;

                listItem = new TreeItem<>(new TermTreeItem(obj, f.listLeftParen()));
                listItem.setExpanded(true);


                if (OtpUtil.isLittleTerm(obj)) {
                    listItem.setValue(new TermTreeItem(obj, f.toString(obj)));
                    parent.getChildren().add(listItem);
                } else {
                    listItem.setValue(new TermTreeItem(obj, f.listLeftParen()));
                    for (final OtpErlangObject e : elements) {
                        this.addToTreeItem(listItem, e);
                    }
                    if (!((OtpErlangList) obj).isProper()) {
                        listItem.getChildren().add(new TreeItem<>(new TermTreeItem(obj, f.cons())));
                        this.addToTreeItem(listItem, ((OtpErlangList) obj).getLastTail());
                    }
                    parent.getChildren().add(listItem);
                    parent.getChildren().add(new TreeItem<>(new TermTreeItem(obj, f.listRightParen())));
                }
            }
        } else if (obj instanceof OtpErlangMap) {
            final TreeItem<TermTreeItem> mapNode = new TreeItem<>(new TermTreeItem(obj, f.mapLeft(obj)));
            parent.getChildren().add(mapNode);
            for (final Map.Entry<OtpErlangObject, OtpErlangObject> e : ((OtpErlangMap) obj).entrySet()) {
                if (f.isHiddenField(e.getKey()).booleanValue()) continue;
                final String keyStr = f.mapKeyToString(e.getKey());
                final String valStr = f.toString(e.getValue());
                // Inline short values that are not maps
                final TreeItem<TermTreeItem> key;
                if (!(e.getValue() instanceof OtpErlangMap) && (50 > valStr.length())) {
                    key = new TreeItem<>(new TermTreeItem(e.getKey(), valStr));
                    key.setGraphic(TermTreeView.recordLabel(keyStr));
                } else {
                    key = new TreeItem<>(new TermTreeItem(e.getKey(), ""));
                    key.setGraphic(TermTreeView.recordLabel(keyStr));
                    this.addToTreeItem(key, e.getValue());
                }
                mapNode.getChildren().add(key);
            }
            parent.getChildren().add(new TreeItem<>(new TermTreeItem(obj, f.mapRight())));
        } else {
            parent.getChildren().add(new TreeItem<>(new TermTreeItem(obj, f.toString(obj))));
        }
    }

    /**
     * return null if this tuple is not a known record, or a string list
     * of record field names if it is.
     */
    private List<String> findRecordDef(final OtpErlangTuple obj) {
        if (0 == obj.arity() || !(obj.elementAt(0) instanceof OtpErlangAtom)) return null;
        final OtpErlangAtom recordName = (OtpErlangAtom) obj.elementAt(0);
        return ErlyBerly.nodeAPI().getRecordManager().get(new RecordManager.RecordKey(this.moduleName, recordName));
    }

    private static Label recordLabel(final String recordNameText) {
        final Label label;
        label = new Label(recordNameText);
        label.getStyleClass().add("record-label");
        return label;
    }

    private static boolean isRecordField(final OtpErlangObject obj) {
        return OtpUtil.isTupleTagged(ERLYBERLY_RECORD_FIELD_ATOM, obj);
    }

    private void onCopyCalls(final ActionEvent e) {
        final StringBuilder sbuilder = new StringBuilder();
        for (final TreeItem item : this.getSelectionModel().getSelectedItems()) {
            this.copyTerms(item, sbuilder);
        }
    }

    private void copyTerms(final TreeItem item, final StringBuilder sbuilder) {
        for (final Object obj : item.getChildren()) {
            this.copyTerms((TreeItem) obj, sbuilder);
        }
        TermTreeView.copyToClipboard(sbuilder);
    }

    private static void copyToClipboard(final StringBuilder sbuilder) {
        final Clipboard clipboard = Clipboard.getSystemClipboard();
        final ClipboardContent content = new ClipboardContent();
        content.putString(sbuilder.toString());
        clipboard.setContent(content);
    }
}
