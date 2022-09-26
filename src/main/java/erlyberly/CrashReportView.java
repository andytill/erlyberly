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

import com.ericsson.otp.erlang.OtpErlangTuple;
import javafx.beans.property.SimpleObjectProperty;
import javafx.scene.control.*;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;

public class CrashReportView extends TabPane {

    private final TermTreeView termTreeView = new TermTreeView();
    private final TermTreeView argsTreeView = new TermTreeView();
    private final StackTraceView stackTraceView = new StackTraceView();
    private final TableView<Object[]> crashInfoTable = new TableView<>();

    public CrashReportView() {
        super();
        VBox.setVgrow(this.termTreeView, Priority.ALWAYS);

        final Label label;
        label = new Label("Stack Trace");
        label.setStyle("-fx-padding: 5; -fx-font-size: 14;");

        final Tab stackTraceTab;
        final Tab argsTermsTab;
        final Tab termsTab;
        termsTab = new Tab("Crash Report Terms");
        termsTab.setContent(this.termTreeView);
        argsTermsTab = new Tab("Call Args");
        argsTermsTab.setContent(this.argsTreeView);
        stackTraceTab = new Tab("Stack Trace");
        stackTraceTab.setContent(new VBox(this.crashInfoTable, label, this.stackTraceView));

        this.getTabs().addAll(stackTraceTab, argsTermsTab, termsTab);
    }

    public void setCrashReport(final CrashReport crashReport) {
        this.stackTraceView.populateFromCrashReport(crashReport);
        this.termTreeView.populateFromTerm(crashReport.getProps());

        final Object[][] crashProps = {{"Pid", crashReport.getPid()}, {"Reg. Name", crashReport.getRegisteredName()}, {"Error", ErlyBerly.getTermFormatter().exceptionToString(crashReport.getErrorClass(), crashReport.getErrorReason())}, {"Initial Call", ErlyBerly.getTermFormatter().modFuncArgsToString((OtpErlangTuple) crashReport.getProcessInitialCall())}};

        final TableColumn<Object[], Object> keyColumn = new TableColumn<>("Key");
        final TableColumn<Object[], Object> valueColumn = new TableColumn<>("Value");

        keyColumn.setCellValueFactory(p -> new SimpleObjectProperty<>((p.getValue()[0])));

        valueColumn.setCellValueFactory(p -> new SimpleObjectProperty<>((p.getValue()[1])));

        this.crashInfoTable.getColumns().add(keyColumn);
        this.crashInfoTable.getColumns().add(valueColumn);

        this.crashInfoTable.getItems().addAll(crashProps);

        crashReport.getCallArgs().ifPresent(this.argsTreeView::populateFromListContents);
    }
}
