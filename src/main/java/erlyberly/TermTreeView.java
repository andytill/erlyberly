package erlyberly;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlyberly.format.TermFormatter;
import erlyberly.node.OtpUtil;
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

        setContextMenu(new ContextMenu(copyMenuItem, dictMenuItem, hexViewMenuItem));
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
                    tupleItem.setGraphic(recordLabel(OtpUtil.tupleElement(1, obj) + " =  "));
                    tupleItem.setExpanded(true);

                    parent.getChildren().add(tupleItem);

                    OtpErlangObject value = OtpUtil.tupleElement(2, obj);
                    if(OtpUtil.isLittleTerm(value))
                        tupleItem.setValue(new TermTreeItem(value, f.toString(obj)));
                    else
                        addToTreeItem(tupleItem, value);
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
                    parent.getChildren().add(listItem);
                    parent.getChildren().add(new TreeItem<>(new TermTreeItem(obj, f.listRightParen())));
                }
            }
        }
        else {
            parent.getChildren().add(new TreeItem<>(new TermTreeItem(obj, f.toString(obj))));
        }
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

        for (TreeItem obj : getSelectionModel().getSelectedItems()) {
            sbuilder.append(obj.getValue()).append("\n");
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
