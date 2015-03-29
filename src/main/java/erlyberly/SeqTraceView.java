package erlyberly;

import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;

public class SeqTraceView extends VBox {

	private final TreeItem<SeqTraceLog> rootItem;
	private TreeView<SeqTraceLog> treeView;

	public SeqTraceView(ObservableList<SeqTraceLog> seqTraceLogs) {
		treeView = new TreeView<SeqTraceLog>(new TreeItem<>());
		treeView.setShowRoot(false);
		treeView.setOnMouseClicked(this::onTraceClicked);
		
		rootItem = treeView.getRoot();

		treeView.setMaxHeight(Integer.MAX_VALUE);
		VBox.setVgrow(treeView, Priority.ALWAYS);
		
		getChildren().add(treeView);
		
		seqTraceLogs.addListener(this::traceLogsChanged);
	}
	
	private void traceLogsChanged(ListChangeListener.Change<? extends SeqTraceLog> e) {
		while(e.next()) {
			for (SeqTraceLog trace : e.getAddedSubList()) {
				rootItem.getChildren().add(new TreeItem<SeqTraceLog>(trace));
			}
		}
	}

	private void onTraceClicked(MouseEvent me) {
		if(me.getButton().equals(MouseButton.PRIMARY)) {
            if(me.getClickCount() == 2) {
            	TreeItem<SeqTraceLog> selectedItem = treeView.getSelectionModel().getSelectedItem();
            	
            	if(selectedItem != null) {
                	showTraceTermView(selectedItem.getValue()); 
            	}
        	}
        }
	}

	private void showTraceTermView(final SeqTraceLog seqTraceLog) {
		
		TermTreeView argTermsTreeView;
		
		argTermsTreeView = newTermTreeView();
		argTermsTreeView.populateFromTerm(seqTraceLog.getMessage());
		
	/*	StringBuilder sb = new StringBuilder(seqTraceLog.getPidString());
		sb.append(" ");
		seqTraceLog.appendFunctionToString(sb);*/
		
		showWindow(argTermsTreeView, "");
	}

	private void showWindow(Parent parent, CharSequence sb) {
		Stage termsStage = new Stage();
		Scene scene  = new Scene(parent);
		
		CloseWindowOnEscape.apply(scene, termsStage);
		
		termsStage.setScene(scene);
        termsStage.setWidth(800);
        termsStage.setHeight(600);
        termsStage.setTitle(sb.toString());
        termsStage.show();
	}

	private TermTreeView newTermTreeView() {
		TermTreeView termTreeView;
		
		termTreeView = new TermTreeView();
		termTreeView.setMaxHeight(Integer.MAX_VALUE);
		VBox.setVgrow(termTreeView, Priority.ALWAYS);
		
		return termTreeView;
	}
}
