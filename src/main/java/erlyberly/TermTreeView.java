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
import java.util.List;
import java.util.Map;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangFun;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangMap;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlyberly.format.TermFormatter;
import erlyberly.node.OtpUtil;
import erlyberly.node.RecordManager;
import hextstar.HexstarView;
import javafx.event.ActionEvent;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.Label;
import javafx.scene.control.MenuItem;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.input.Clipboard;
import javafx.scene.input.ClipboardContent;
import javafx.scene.input.KeyCombination;

@SuppressWarnings("rawtypes")
public class TermTreeView extends TreeView<TermTreeItem> {

    private static final OtpErlangAtom DICT_ATOM = OtpUtil.atom("dict");
    private static final OtpErlangAtom ERLYBERLY_RECORD_FIELD_ATOM = OtpUtil.atom("erlyberly_record_field");
    private OtpErlangAtom moduleName;

    public TermTreeView() {
        getStyleClass().add("term-tree");
        setRoot(new TreeItem<TermTreeItem>());

        MenuItem copyMenuItem = new MenuItem("Copy");
        copyMenuItem.setAccelerator(KeyCombination.keyCombination("shortcut+c"));
        copyMenuItem.setOnAction(this::onCopyCalls);

        MenuItem dictMenuItem = new MenuItem("Dict to List");
        dictMenuItem.setOnAction(this::onViewDict);

        MenuItem hexViewMenuItem = new MenuItem("Hex View");
        hexViewMenuItem.setOnAction(this::onHexView);

        MenuItem decompileFunContextMenu = new MenuItem("Decompile Fun");
        decompileFunContextMenu.setOnAction(this::onDecompileFun);

        setContextMenu(new ContextMenu(copyMenuItem, dictMenuItem, decompileFunContextMenu, hexViewMenuItem));
    }

    public void onHexView(ActionEvent e) {
        TreeItem<TermTreeItem> item = getSelectionModel().getSelectedItem();
        if(item != null && item.getValue().getObject() instanceof OtpErlangBinary) {
            OtpErlangBinary binary = (OtpErlangBinary) item.getValue().getObject();
            HexstarView hexstarView;
            hexstarView = new HexstarView();
            hexstarView.setBinary(binary);
            ErlyBerly.showPane("Hex View", ErlyBerly.wrapInPane(hexstarView));
        }
    }

    public void onDecompileFun(ActionEvent e) {
        TreeItem<TermTreeItem> item = getSelectionModel().getSelectedItem();
        if(item != null && item.getValue().getObject() instanceof OtpErlangFun) {
            OtpErlangFun fun = (OtpErlangFun)item.getValue().getObject();
            try {
                String funSource = ErlyBerly.nodeAPI().decompileFun(fun);
                ErlyBerly.showPane(fun + " Source Code", ErlyBerly.wrapInPane(new CodeView(funSource)));
            } catch (OtpErlangException e1) {
                e1.printStackTrace();
            } catch (IOException e1) {
                e1.printStackTrace();
            }
        }
    }

    public void onViewDict(ActionEvent e) {
        TreeItem<TermTreeItem> item = getSelectionModel().getSelectedItem();
        if(item == null)
            return;
        if(item.getValue() == null || !DICT_ATOM.equals(item.getValue().getObject()))
            return;
        if(item.getParent() == null || !(item.getParent().getValue().getObject() instanceof OtpErlangTuple))
            return;

        OtpErlangObject dict = item.getParent().getValue().getObject();

        try {
            OtpErlangList props = ErlyBerly.nodeAPI().dictToPropslist(dict);
            TermTreeView parentControl = new TermTreeView();
            parentControl.populateFromTerm(props);
            ErlyBerly.showPane("dict_to_list", ErlyBerly.wrapInPane(parentControl));
        }
        catch (Exception e1) {
            e1.printStackTrace();
        }
    }

    public void populateFromListContents(OtpErlangList list) {
        for (OtpErlangObject a : list) {
            populateFromTerm(a);
        }
    }

    public void populateFromTerm(OtpErlangObject obj) {
        populateFromTerm(null, obj);
    }


    public void populateFromTerm(OtpErlangAtom moduleName, OtpErlangObject obj) {
        this.moduleName = moduleName;
        setShowRoot(false);
        addToTreeItem(getRoot(), obj);
    }

    private void addToTreeItem(TreeItem<TermTreeItem> parent, OtpErlangObject obj) {
        TermFormatter f = ErlyBerly.getTermFormatter();

        if(obj instanceof OtpErlangBinary) {
            String termString = f.toString(obj);
            TreeItem<TermTreeItem> item = new TreeItem<>(new TermTreeItem(obj, termString));
            parent.getChildren().add(item);
        }
        else if(obj instanceof OtpErlangTuple) {
            OtpErlangObject[] elements = ((OtpErlangTuple) obj).elements();

            if(elements.length == 0) {
                parent.getChildren().add(new TreeItem<>(new TermTreeItem(obj, f.emptyTupleString())));
            }
            else {
                TreeItem<TermTreeItem> tupleItem;
                if(OtpUtil.isErlyberlyRecord(obj)) {
                    // this is the "old" way show tuples with record metadata, where the metadata is in the
                    // term itself, rather than stored separately in the RecordManager
                    String recordNameText = "#" + OtpUtil.tupleElement(1, obj) + " ";

                    tupleItem = new TreeItem<>(new TermTreeItem(obj, f.tupleLeftParen()));
                    tupleItem.setGraphic(recordLabel(recordNameText));
                    parent.getChildren().add(tupleItem);
                    tupleItem.setExpanded(true);
                    elements = OtpUtil.iterableElements(OtpUtil.tupleElement(2, obj));
                    for (OtpErlangObject e : elements) {
                        addToTreeItem(tupleItem, e);
                    }
                    parent.getChildren().add(new TreeItem<>(new TermTreeItem(obj, f.tupleRightParen())));
                }
                else if(isRecordField(obj)) {
                    tupleItem = new TreeItem<>(new TermTreeItem(obj, " "));
                    OtpErlangString recordField = (OtpErlangString) OtpUtil.tupleElement(1, obj);
                    tupleItem.setGraphic(recordLabel(recordField.stringValue() + " =  "));
                    tupleItem.setExpanded(true);

                    parent.getChildren().add(tupleItem);

                    OtpErlangObject value = OtpUtil.tupleElement(2, obj);
                    if(OtpUtil.isLittleTerm(value))
                        tupleItem.setValue(new TermTreeItem(value, f.toString(obj)));
                    else
                        addToTreeItem(tupleItem, value);
                }
                else {
                    List<String> recordNames = null;
                    OtpErlangTuple objTuple = (OtpErlangTuple) obj;
                    if((recordNames = findRecordDef(objTuple)) != null) {
                        String recordNameText = "#" + OtpUtil.tupleElement(0, obj) + " ";
                        tupleItem = new TreeItem<>(new TermTreeItem(obj, f.tupleLeftParen()));
                        tupleItem.setGraphic(recordLabel(recordNameText));
                        parent.getChildren().add(tupleItem);
                        tupleItem.setExpanded(true);
                        elements = objTuple.elements();
                        for (int i = 1; i < elements.length; i++) {
                            addToTreeItem(tupleItem, OtpUtil.tuple(ERLYBERLY_RECORD_FIELD_ATOM, recordNames.get(i-1), elements[i]));
                        }
                        parent.getChildren().add(new TreeItem<>(new TermTreeItem(obj, f.tupleRightParen())));
                    }
                    else {
                        tupleItem = new TreeItem<>();
                        tupleItem.setExpanded(true);
                        if(OtpUtil.isLittleTerm(obj)) {
                            tupleItem.setValue(new TermTreeItem(obj, f.toString(obj)));
                            parent.getChildren().add(tupleItem);
                        }
                        else {
                            tupleItem.setValue(new TermTreeItem(obj, f.tupleLeftParen()));
                            for (OtpErlangObject e : elements) {
                                addToTreeItem(tupleItem, e);
                            }
                            parent.getChildren().add(tupleItem);
                            parent.getChildren().add(new TreeItem<>(new TermTreeItem(obj, f.tupleRightParen())));
                        }
                    }
                }
            }
        }
        else if(obj instanceof OtpErlangList) {
            OtpErlangObject[] elements = ((OtpErlangList) obj).elements();

            if(elements.length == 0) {
                parent.getChildren().add(new TreeItem<>(new TermTreeItem(obj, f.emptyListString())));
            }
            else {
                TreeItem<TermTreeItem> listItem;

                listItem = new TreeItem<>(new TermTreeItem(obj, f.listLeftParen()));
                listItem.setExpanded(true);


                if(OtpUtil.isLittleTerm(obj)) {
                    listItem.setValue(new TermTreeItem(obj, f.toString(obj)));
                    parent.getChildren().add(listItem);
                }
                else {
                    listItem.setValue(new TermTreeItem(obj, f.listLeftParen()));
                    for (OtpErlangObject e : elements) {
                        addToTreeItem(listItem, e);
                    }
                    if (!((OtpErlangList)obj).isProper()) {
                        listItem.getChildren().add(new TreeItem<>(new TermTreeItem(obj, f.cons())));
                        addToTreeItem(listItem, ((OtpErlangList)obj).getLastTail());
                    }
                    parent.getChildren().add(listItem);
                    parent.getChildren().add(new TreeItem<>(new TermTreeItem(obj, f.listRightParen())));
                }
            }
        }
        else if (obj instanceof OtpErlangMap){
            TreeItem<TermTreeItem> mapNode = new TreeItem<>(new TermTreeItem(obj, f.mapLeft(obj)));
            parent.getChildren().add(mapNode);
            for (Map.Entry<OtpErlangObject,OtpErlangObject> e : ((OtpErlangMap) obj).entrySet()) {
                if (f.isHiddenField(e.getKey()))
                    continue;
                String keyStr = f.mapKeyToString(e.getKey());
                String valStr = f.toString(e.getValue());
                if (valStr.length() < 50) {
                    TreeItem<TermTreeItem> key = new TreeItem<>(new TermTreeItem(e.getKey(), valStr));
                    key.setGraphic(recordLabel(keyStr));
                    mapNode.getChildren().add(key);
                }
                else {
                    TreeItem<TermTreeItem> key = new TreeItem<>(new TermTreeItem(e.getKey(), ""));
                    key.setGraphic(recordLabel(keyStr));
                    addToTreeItem(key, e.getValue());
                    mapNode.getChildren().add(key);
                }
            }
            parent.getChildren().add(new TreeItem<>(new TermTreeItem(obj, f.mapRight())));
        }
        else {
            parent.getChildren().add(new TreeItem<>(new TermTreeItem(obj, f.toString(obj))));
        }
    }

    /**
     * return null if this tuple is not a known record, or a string list
     * of record field names if it is.
     */
    private List<String> findRecordDef(OtpErlangTuple obj) {
        if(obj.arity() == 0 || !(obj.elementAt(0) instanceof OtpErlangAtom))
            return null;
        OtpErlangAtom recordName = (OtpErlangAtom) obj.elementAt(0);
        return ErlyBerly.nodeAPI().getRecordManager().get(new RecordManager.RecordKey(moduleName, recordName));
    }

    private Label recordLabel(String recordNameText) {
        Label label;
        label = new Label(recordNameText);
        label.getStyleClass().add("record-label");
        return label;
    }

    private boolean isRecordField(OtpErlangObject obj) {
        return OtpUtil.isTupleTagged(ERLYBERLY_RECORD_FIELD_ATOM, obj);
    }

    private void onCopyCalls(ActionEvent e) {
        StringBuilder sbuilder = new StringBuilder();
        for (TreeItem item : getSelectionModel().getSelectedItems()) {
            copyTerms(item, sbuilder);
        }
    }

    private void copyTerms(TreeItem item, StringBuilder sbuilder) {
        for (Object obj : item.getChildren()) {
            copyTerms((TreeItem) obj, sbuilder);
        }
        copyToClipboard(sbuilder);
    }

    private void copyToClipboard(StringBuilder sbuilder) {
        final Clipboard clipboard = Clipboard.getSystemClipboard();
        final ClipboardContent content = new ClipboardContent();
        content.putString(sbuilder.toString());
        clipboard.setContent(content);
    }
}
