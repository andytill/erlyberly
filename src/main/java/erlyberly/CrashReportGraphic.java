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

import javafx.scene.control.Label;
import javafx.scene.layout.VBox;

import java.time.format.DateTimeFormatter;

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
        // #75 make sure that the label does not have unbounded width
        label.setMaxWidth(500d);
        label.setStyle("-fx-font-size: 11; -fx-padding: 2 0 0 0;");
        return label;
    }
}
