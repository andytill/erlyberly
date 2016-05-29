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
package ui;

import static de.jensd.fx.fontawesome.AwesomeDude.FONT_AWESOME_TTF_PATH;

import de.jensd.fx.fontawesome.AwesomeDude;
import de.jensd.fx.fontawesome.AwesomeIcon;
import javafx.beans.NamedArg;
import javafx.fxml.FXML;
import javafx.scene.control.Label;
import javafx.scene.text.Font;

/**
 * This is mostly a copy/pasta of the Icon class from fontawesomefx but checks that style
 * is not null before appending it to the style string. Otherwise the css parser throws
 * an error because the string 'null 'is appended to the css.
 *
 * erlyberly has not upgraded to the latest fontawesomefx because the Icon class now
 * inherits from Text and not Label so tooltips cannot be set.
 */
public class FAIcon extends Label {

    @FXML
    void initialize() {
    }

    static {
        Font.loadFont(AwesomeDude.class.getResource(FONT_AWESOME_TTF_PATH).toExternalForm(), 10.0);
    }

    public FAIcon(AwesomeIcon icon, String size, String style, String styleClass) {
        setText(icon.toString());
        // add least add "awesome"-class
        getStyleClass().add("awesome");
        if (styleClass != null && !styleClass.isEmpty()) {
            getStyleClass().add(styleClass);
        }
        size = (size == null || size.isEmpty()) ? "2em" : size;
        // make sure FontAwesome is assigned with appropriate size:
        String stylePrefix = String.format("-fx-font-family: FontAwesome; -fx-font-size: %s;", size);
        if(style != null) {
            stylePrefix += style;
        }
        setStyle(stylePrefix);
    }

    public FAIcon(@NamedArg("awesomeIcon") String awesomeIcon, @NamedArg("size") String size, @NamedArg("style") String style, @NamedArg("styleClass") String styleClass) {
        this(AwesomeIcon.valueOf(awesomeIcon), size, style, styleClass);
    }

    public FAIcon(@NamedArg("awesomeIcon") String awesomeIcon, @NamedArg("size") String size, @NamedArg("styleClass") String styleClass) {
        this(AwesomeIcon.valueOf(awesomeIcon), size, null, styleClass);
    }

    public FAIcon(@NamedArg("awesomeIcon") String awesomeIcon, @NamedArg("size") String size) {
        this(AwesomeIcon.valueOf(awesomeIcon), size, null, null);
    }

    public FAIcon(@NamedArg("awesomeIcon") String awesomeIcon) {
        this(AwesomeIcon.valueOf(awesomeIcon), "2em", null, null);
    }

    private FAIcon() {
        this(AwesomeIcon.STAR, "2em", null, null);
    }

    public static FAIcon create() {
        return new FAIcon();
    }

    public FAIcon icon(AwesomeIcon icon) {
        setText(icon.toString());
        return this;
    }

    public FAIcon size(String iconSize) {
        setStyle("-fx-font-size: " + iconSize + ";");
        return this;
    }

    public FAIcon style(String style) {
        setStyle(getStyle() + style);
        return this;
    }

    public FAIcon styleClass(String styleClass) {
        getStyleClass().add(styleClass);
        return this;
    }
}
