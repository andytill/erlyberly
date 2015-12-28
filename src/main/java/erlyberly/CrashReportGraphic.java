package erlyberly;

import java.time.format.DateTimeFormatter;

import javafx.scene.control.Label;
import javafx.scene.layout.VBox;

/**
 * A control summarising a {@link CrashReport} for button graphics.
 */
public class CrashReportGraphic extends VBox {
    public CrashReportGraphic(CrashReport report) {
        getChildren().addAll(
            dateLabel(report),
            descriptionLabel(report)
        );
    }

    private Label dateLabel(CrashReport report) {
        String dateString = report.getDateTime().format(DateTimeFormatter.ISO_TIME);
        Label label;
        label = new Label(dateString);
        // the date label is grey bold text that is slightly smaller than the main label
        label.setStyle("-fx-font-size: 10; -fx-font-weight: bold; -fx-text-fill: #b2b2b2;");
        return label;
    }

    private Label descriptionLabel(CrashReport report) {
        String processString = report.getRegisteredName();
        if("".equals(processString)) {
            processString = report.getPid().toString();
        }
        processString += " " + report.getErrorClass() + ":" + report.getErrorReason();
        Label label;
        label = new Label(processString);
        label.setStyle("-fx-font-size: 11; -fx-padding: 2 0 0 0;");
        return label;
    }
}
