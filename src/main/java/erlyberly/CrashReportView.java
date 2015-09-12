package erlyberly;

import com.ericsson.otp.erlang.OtpErlangObject;

import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;

public class CrashReportView extends VBox {
    
    private final TermTreeView termTreeView;

    public CrashReportView() {
        termTreeView = new TermTreeView();
        
        VBox.setVgrow(termTreeView, Priority.ALWAYS);
        
        getChildren().add(termTreeView);
    }
    
    public void setCrashReport(OtpErlangObject obj) {
        termTreeView.populateFromTerm(obj);
    }
}
