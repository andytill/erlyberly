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

import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;

import java.io.IOException;
import java.net.URL;

class FxmlLoadable {
    final String resource;
    Parent fxmlNode;
    Object controller;

    FxmlLoadable(final String aResource) {
        super();
        this.resource = aResource;
    }

    public Parent load() {
        if (null != this.fxmlNode) return this.fxmlNode;

        final URL location = this.getClass().getResource(this.resource);
        final FXMLLoader fxmlLoader = new FXMLLoader(location);

        try {
            this.fxmlNode = fxmlLoader.load();
            this.controller = fxmlLoader.getController();
        } catch (final IOException e) {
            throw new RuntimeException("Cannot load FXML", e);
        }
        return this.fxmlNode;
    }
}
