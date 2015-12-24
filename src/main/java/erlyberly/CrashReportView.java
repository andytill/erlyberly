package erlyberly;

import java.util.HashMap;

import com.ericsson.otp.erlang.OtpErlangList;

import erlyberly.node.OtpUtil;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableColumn.CellDataFeatures;
import javafx.scene.control.TableView;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.util.Callback;

public class CrashReportView extends TabPane {
    
    private final TermTreeView termTreeView = new TermTreeView();
    private final ListView<StackTraceElement> stackTraceListView = new ListView<>();
    private final TableView<Object[]> crashInfoTable = new TableView<>();

    public CrashReportView() {
        Tab stackTraceTab, termsTab;
        
        VBox.setVgrow(termTreeView, Priority.ALWAYS);
        

        Label label = new Label("Stack Trace");
        label.setStyle("-fx-padding: 5; -fx-font-size: 14;");
        
        termsTab = new Tab("Crash Report Terms", termTreeView);
        stackTraceTab = new Tab("Stack Trace", new VBox(crashInfoTable, label, stackTraceListView));

        getTabs().addAll(stackTraceTab, termsTab);
    }

    public void setCrashReport(OtpErlangList obj) {
        System.out.println(obj);
        HashMap<Object, Object> crashMap  = OtpUtil.propsToMap(obj);
        CrashReport crashReport  = new CrashReport(crashMap);
        try {
            stackTraceListView.getItems().addAll(crashReport.mapStackTraces((module, function, arity, file, line) -> {
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
        
        Object[][] crashProps = {
                {"Pid", crashMap.get(OtpUtil.atom("pid")) },
                {"Reg. Name", crashMap.get(OtpUtil.atom("registered_name")) },
                {"Initial Call", crashMap.get(OtpUtil.atom("initial_call")) }};

        TableColumn<Object[], Object> keyColumn = new TableColumn<>("Key");
        TableColumn<Object[], Object> valueColumn = new TableColumn<>("Value");
        
        keyColumn.setCellValueFactory(new Callback<CellDataFeatures<Object[], Object>, ObservableValue<Object>>() {
            @Override
            public ObservableValue<Object> call(CellDataFeatures<Object[], Object> p) {
                return new SimpleObjectProperty<>((p.getValue()[0]));
            }
        });
        

        
        valueColumn.setCellValueFactory(new Callback<CellDataFeatures<Object[], Object>, ObservableValue<Object>>() {
            @Override
            public ObservableValue<Object> call(CellDataFeatures<Object[], Object> p) {
                return new SimpleObjectProperty<>((p.getValue()[1]));
            }
        });
        
        crashInfoTable.getColumns().add(keyColumn);
        crashInfoTable.getColumns().add(valueColumn);

        crashInfoTable.getItems().addAll(crashProps);
    }

    private class StackTraceElement {
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
