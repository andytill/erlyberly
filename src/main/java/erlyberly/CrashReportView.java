package erlyberly;

import com.ericsson.otp.erlang.OtpErlangTuple;

import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.Label;
import javafx.scene.control.ListCell;
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
    private final TermTreeView argsTreeView = new TermTreeView();
    private final ListView<StackTraceElement> stackTraceListView = new ListView<>();
    private final TableView<Object[]> crashInfoTable = new TableView<>();

    public CrashReportView() {        
        VBox.setVgrow(termTreeView, Priority.ALWAYS);

        Label label;
        label = new Label("Stack Trace");
        label.setStyle("-fx-padding: 5; -fx-font-size: 14;");

        Tab stackTraceTab, argsTermsTab, termsTab;
        termsTab = new Tab("Crash Report Terms");
        termsTab.setContent(termTreeView);
        argsTermsTab = new Tab("Call Args");
        argsTermsTab.setContent(argsTreeView);
        stackTraceTab = new Tab("Stack Trace");
        stackTraceTab.setContent(new VBox(crashInfoTable, label, stackTraceListView));

        getTabs().addAll(stackTraceTab, argsTermsTab, termsTab);
        
        stackTraceListView.setCellFactory(new Callback<ListView<StackTraceElement>, ListCell<StackTraceElement>>() {
            @Override
            public ListCell<StackTraceElement> call(ListView<StackTraceElement> param) {
                return new ListCell<StackTraceElement>() {
                    private final Hyperlink functionLink = new Hyperlink();
                    {
                        setGraphic(functionLink);
                        functionLink.setOnAction((e) -> {
                            StackTraceElement stackElement = getItem();
                            if(stackElement == null)
                                return;
                            ModFunc mf = stackElement.getModFunc();
                            try {
                                String source = ErlyBerly.nodeAPI().moduleFunctionSourceCode(
                                        mf.getModuleName(), mf.getFuncName(), mf.getArity());
                                ErlyBerly.showPane(
                                    "Crash Report Stack",
                                    ErlyBerly.wrapInPane(new CodeView(source))
                                );
                            } catch (Exception e1) {
                                e1.printStackTrace();
                            } 
                        });
                    }
                    
                    @Override
                    public void updateItem(StackTraceElement item, boolean empty) {
                        super.updateItem(item, empty);
                        if(item == null)
                            functionLink.setText("");
                        else
                            functionLink.setText(item.toString());
                    }
                };
            }
        });
    }

    public void setCrashReport(CrashReport crashReport) {
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
        termTreeView.populateFromTerm(crashReport.getProps());
        
        Object[][] crashProps = {
                {"Pid", crashReport.getPid() },
                {"Reg. Name", crashReport.getRegisteredName() },
                {"Error", ErlyBerly.getTermFormatter().exceptionToString(crashReport.getErrorClass(), crashReport.getErrorReason()) },
                {"Initial Call", ErlyBerly.getTermFormatter().mfaToString((OtpErlangTuple) crashReport.getProcessInitialCall()) }};

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
        
        crashReport.getCallArgs().ifPresent((callArgs) -> {
            argsTreeView.populateFromListContents(callArgs);
        });
    }

    private class StackTraceElement {
        private final ModFunc modFunc;
        private final long line;
        private final String file;

        public StackTraceElement(ModFunc modFunc, String file, long line) {
            this.modFunc = modFunc;
            this.file = file;
            this.line = line;
        }

        public ModFunc getModFunc() {
            return modFunc;
        }

        @Override
        public String toString() {
            return modFunc.toFullString() + "  (" + file + ":" + line +")";
        }
    }
}
