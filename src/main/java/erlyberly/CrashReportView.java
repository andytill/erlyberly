package erlyberly;

import com.ericsson.otp.erlang.OtpErlangList;

import erlyberly.node.OtpUtil;
import javafx.scene.control.ListView;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;

public class CrashReportView extends TabPane {
    
    private final TermTreeView termTreeView = new TermTreeView();
    private final ListView<StackTraceElement> stackTrace = new ListView<>();

    public CrashReportView() {
        Tab stackTraceTab, termsTab;
        
        VBox.setVgrow(termTreeView, Priority.ALWAYS);
        
        termsTab = new Tab("Crash Report Terms", termTreeView);
        stackTraceTab = new Tab("Stack Trace", stackTrace);
        getTabs().addAll(stackTraceTab, termsTab);
    }

    public void setCrashReport(OtpErlangList obj) {
        CrashReport crashReport  = new CrashReport(OtpUtil.propsToMap(obj));
        try {
            stackTrace.getItems().addAll(crashReport.mapStackTraces((module, function, arity, file, line) -> {
                try {
                    boolean exported = false;
                    boolean synthetic = false;
                    ModFunc modFunc = new ModFunc(module.toString(), function.toString(), arity.intValue(), exported, synthetic);
                    return new StackTraceElement(modFunc, file.toString(), line.longValue());
                }
                catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }));
        } catch (Exception e) {
            e.printStackTrace();
        }
        termTreeView.populateFromTerm(obj);
    }

    class StackTraceElement {
        private ModFunc modFunc;
        private long line;
        private String file;

        public StackTraceElement(ModFunc modFunc, String file, long line) {
            this.modFunc = modFunc;
            this.file = file;
            this.line = line;
        }

        @Override
        public String toString() {
            return modFunc.toFullString() + "  (" + file + ":" + line +")";
        }
    }
}
