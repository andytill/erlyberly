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

import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.scene.control.Label;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.HBox;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.Glyph;
import ui.CellController;

class ModFuncGraphic extends HBox implements CellController<ModFunc> {

    @FunctionalInterface
    public interface TraceFn {
        void trace(ModFunc modFunc);
    }

    @FunctionalInterface
    public interface IsTracedFn {
        boolean isTraced(ModFunc mf);
    }

    private static final String ICON_STYLE = "-fx-font-family: FontAwesome; -fx-font-size: 1em;";

    private final SimpleStringProperty text = new SimpleStringProperty();

    private final SimpleStringProperty exportIconText = new SimpleStringProperty();

    private final SimpleStringProperty exportToolTipText = new SimpleStringProperty();

    private final SimpleStringProperty tracedIconText = new SimpleStringProperty(FontAwesome.Glyph.STAR_ALT.toString());

    private final SimpleBooleanProperty tracable = new SimpleBooleanProperty();

    private final TraceFn traceFn;

    private final IsTracedFn isTracedFn;

    private boolean showModuleName;

    private ModFunc modFunc;

    ModFuncGraphic(final TraceFn aTraceFn, final IsTracedFn isTracedFn) {
        super();
        this.traceFn = aTraceFn;
        this.isTracedFn = isTracedFn;

        this.getStyleClass().add("mod-func-graphic");

        this.getChildren().addAll(this.exportIconGraphic(), this.traceIcon(), this.functionLabel());
    }

    private Glyph exportIconGraphic() {
        final Tooltip tooltip;

        tooltip = new Tooltip();
        tooltip.textProperty().bindBidirectional(this.exportToolTipText);

        final Glyph treeIcon;

        treeIcon = ModFuncGraphic.treeIcon(FontAwesome.Glyph.SQUARE);
        treeIcon.textProperty().bindBidirectional(this.exportIconText);
        treeIcon.setTooltip(tooltip);
        return treeIcon;
    }

    private Glyph traceIcon() {
        final Glyph traceIcon;

        traceIcon = new FontAwesome().create(ICON_STYLE);
        traceIcon.textProperty().bind(this.tracedIconText);
        traceIcon.visibleProperty().bind(this.tracable);
        traceIcon.setTooltip(new Tooltip("Toggle tracing, double click on this star or ctrl+t when selected"));
        traceIcon.getStyleClass().add("erlyberly-icon-button");
        traceIcon.setOnMouseClicked((e) -> {
            if (2 == e.getClickCount()) this.traceFn.trace(this.modFunc);
        });
        return traceIcon;
    }

    private Label functionLabel() {
        final Label label;

        label = new Label();
        label.textProperty().bind(this.text);

        return label;
    }

    private static Glyph treeIcon(final FontAwesome.Glyph treeIcon) {
        final Glyph icon = new FontAwesome().create(treeIcon);
        icon.setStyle(ICON_STYLE);
        return icon;
    }

    @Override
    public void updateItem(final ModFunc item, final boolean empty) {
        if (null == item || empty) {
            this.text.set(null);
        } else {
            if (this.showModuleName) this.text.set(item.toFullString());
            else this.text.set(item.toString());

            this.updateExportIcon(item);

            // no tracing of the whole module for now!
            this.tracable.set(!item.isModule());
        }
        this.modFunc = item;

        this.onTracesChange();
    }

    private void updateExportIcon(final ModFunc item) {
        final Glyph icon;
        final String tooltipText;

        if (item.isModule()) {
            tooltipText = "Module";
            icon = new FontAwesome().create(FontAwesome.Glyph.CUBE);
        } else if (item.isExported()) {
            tooltipText = "Exported function";
            icon = new FontAwesome().create(FontAwesome.Glyph.UNLOCK_ALT);
        } else {
            tooltipText = "Unexported function";
            icon = new FontAwesome().create(FontAwesome.Glyph.LOCK);
        }

        this.exportToolTipText.set(tooltipText);
        this.exportIconText.set(icon.getText());
    }

    public void onTracesChange() {
        if (null != this.modFunc && this.isTracedFn.isTraced(this.modFunc))
            this.tracedIconText.set(new FontAwesome().create(FontAwesome.Glyph.CHECK_SQUARE_ALT).getText());
        else this.tracedIconText.set(new FontAwesome().create(FontAwesome.Glyph.SQUARE_ALT).getText());
    }

    public boolean isShowModuleName() {
        return this.showModuleName;
    }

    public void setShowModuleName(final boolean showModuleName) {
        this.showModuleName = showModuleName;
    }
}
