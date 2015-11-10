package erlyberly;

import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlyberly.node.OtpUtil;
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

	public TermTreeView() {
		setRoot(new TreeItem<TermTreeItem>());

		MenuItem copyMenuItem = new MenuItem("Copy");
		copyMenuItem.setAccelerator(KeyCombination.keyCombination("shortcut+c"));
		copyMenuItem.setOnAction(this::onCopyCalls);

        MenuItem dictMenuItem = new MenuItem("Dict to List");
        dictMenuItem.setOnAction(this::onViewDict);

        setContextMenu(new ContextMenu(copyMenuItem, dictMenuItem));
}

    public void onViewDict(ActionEvent e) {
        TreeItem<TermTreeItem> item = getSelectionModel().getSelectedItem();
        if(item == null)
            return;
        if(item.getValue() == null || !OtpUtil.atom("dict").equals(item.getValue().getObject()))
            return;
        if(item.getParent() == null || !(item.getParent().getValue().getObject() instanceof OtpErlangTuple))
            return;

        OtpErlangObject dict = item.getParent().getValue().getObject();

        try {
            OtpErlangList props = ErlyBerly.nodeAPI().dictToPropslist(dict);
            TermTreeView parentControl = new TermTreeView();
            parentControl.populateFromTerm(props);
            ErlyBerly.subWindow("dict_to_list", parentControl);
        }
        catch (Exception e1) {
            e1.printStackTrace();
        }
    }

	public void populateFromTerm(OtpErlangObject obj) {
		setShowRoot(false);
		addToTreeItem(getRoot(), obj);
	}
	
	private void addToTreeItem(TreeItem<TermTreeItem> parent, OtpErlangObject obj) {
		if(obj instanceof OtpErlangBinary) {
			TreeItem<TermTreeItem> item = new TreeItem<>(new TermTreeItem(obj, OtpUtil.binaryToString((OtpErlangBinary) obj)));
			parent.getChildren().add(item);
		}
		else if(obj instanceof OtpErlangTuple) {
			OtpErlangObject[] elements = ((OtpErlangTuple) obj).elements();
			
			if(elements.length == 0) {
				parent.getChildren().add(new TreeItem<>(new TermTreeItem(obj, "{ }")));
			}
			else {
                TreeItem<TermTreeItem> tupleItem;
                if(OtpUtil.isErlyberlyRecord(obj)) {
                    String recordNameText = "#" + OtpUtil.tupleElement(1, obj) + " ";
                    
                    tupleItem = new TreeItem<>(new TermTreeItem(obj, "{"));
                    tupleItem.setGraphic(recordLabel(recordNameText));
                    parent.getChildren().add(tupleItem);
                    tupleItem.setExpanded(true);
                    elements = OtpUtil.iterableElements(OtpUtil.tupleElement(2, obj));
                    for (OtpErlangObject e : elements) {
                        addToTreeItem(tupleItem, e);
                    }
                    parent.getChildren().add(new TreeItem<>(new TermTreeItem(obj, "}")));
                }
                else if(isRecordField(obj)) {
                    tupleItem = new TreeItem<>(new TermTreeItem(obj, " "));
                    tupleItem.setGraphic(recordLabel(OtpUtil.tupleElement(1, obj) + " =  "));
                    tupleItem.setExpanded(true);

                    parent.getChildren().add(tupleItem);

                    OtpErlangObject value = OtpUtil.tupleElement(2, obj);
                    if(OtpUtil.isLittleTerm(value))
                        tupleItem.setValue(new TermTreeItem(value, otpObjectToString(value)));
                    else
                        addToTreeItem(tupleItem, value);
				}
				else {
					tupleItem = new TreeItem<>();
					tupleItem.setExpanded(true);
					
					if(OtpUtil.isLittleTerm(obj)) {
					    tupleItem.setValue(new TermTreeItem(obj, otpObjectToString(obj)));
	                    parent.getChildren().add(tupleItem);
					}
					else {
                        tupleItem.setValue(new TermTreeItem(obj, "{"));
	                    for (OtpErlangObject e : elements) {
	                        addToTreeItem(tupleItem, e);
	                    }
	                    parent.getChildren().add(tupleItem);
	                    parent.getChildren().add(new TreeItem<>(new TermTreeItem(obj, "}")));
					}
					
				}
			}
		}
		else if(obj instanceof OtpErlangList) {
			OtpErlangObject[] elements = ((OtpErlangList) obj).elements();
			
			if(elements.length == 0) {
                parent.getChildren().add(new TreeItem<>(new TermTreeItem(obj, "[ ]")));
			}
			else {
				TreeItem<TermTreeItem> listItem;
				
				listItem = new TreeItem<>(new TermTreeItem(obj, "["));
				listItem.setExpanded(true);
				

                if(OtpUtil.isLittleTerm(obj)) {
                    listItem.setValue(new TermTreeItem(obj, otpObjectToString(obj)));
                    parent.getChildren().add(listItem);
                }
                else {
                    listItem.setValue(new TermTreeItem(obj, "["));
                    for (OtpErlangObject e : elements) {
                        addToTreeItem(listItem, e);
                    }
                    parent.getChildren().add(listItem);
                    parent.getChildren().add(new TreeItem<>(new TermTreeItem(obj, "]")));
                }
			}
		}
		else {
            parent.getChildren().add(new TreeItem<>(new TermTreeItem(obj, otpObjectToString(obj))));
		}
	}

    private String otpObjectToString(OtpErlangObject obj) {
        StringBuilder stringBuilder = new StringBuilder();
        OtpUtil.otpObjectToString(obj, stringBuilder);
        String string = stringBuilder.toString();
        return string;
    }

    private Label recordLabel(String recordNameText) {
        Label label;
        label = new Label(recordNameText);
        label.getStyleClass().add("record-label");
        return label;
    }

	private boolean isRecordField(OtpErlangObject obj) {
		return OtpUtil.isTupleTagged(OtpUtil.atom("erlyberly_record_field"), obj);
	}

    public void populateFromListContents(OtpErlangList list) {
        for (OtpErlangObject a : list) {
            populateFromTerm(a);
        }
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
