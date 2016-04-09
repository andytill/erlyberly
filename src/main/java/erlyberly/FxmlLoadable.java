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

import java.io.IOException;
import java.net.URL;

import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;

class FxmlLoadable {
    final String resource;
    Parent fxmlNode;
    Object controller;

    public FxmlLoadable(String aResource) {
        resource = aResource;
    }

    public Parent load() {
        if(fxmlNode != null)
            return fxmlNode;

        URL location = getClass().getResource(resource);
        FXMLLoader fxmlLoader = new FXMLLoader(location);

        try {
            fxmlNode = (Parent) fxmlLoader.load();
            controller = fxmlLoader.getController();
        } catch (IOException e) {
            throw new RuntimeException("Cannot load FXML", e);
        }
        return fxmlNode;
    }
}
