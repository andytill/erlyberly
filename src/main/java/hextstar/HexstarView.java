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
package hextstar;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Arrays;

import com.ericsson.otp.erlang.OtpErlangBinary;

import javafx.scene.control.SelectionMode;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.PropertyValueFactory;

/**
 * Created by Bart on 9/26/2015.
 * Originally from https://github.com/Velocity-/Hexstar
 */
public class HexstarView extends TableView<DataRow> {

    public HexstarView() {
        //setContextMenu(new ContextMenu(new MenuItem("Copy"), new MenuItem("Cut"), new MenuItem("Delete")));
        getSelectionModel().setCellSelectionEnabled(true);
        getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);

        column("Address", "address");
        column("0", "b0");
        column("1", "b1");
        column("2", "b2");
        column("3", "b3");
        column("4", "b4");
        column("5", "b5");
        column("6", "b6");
        column("7", "b7");
        column("8", "b8");
        column("9", "b9");
        column("A", "bA");
        column("B", "bB");
        column("C", "bC");
        column("D", "bD");
        column("E", "bE");
        column("F", "bF");
        column("Text", "text");

        getColumns().forEach(c -> {
            c.getStyleClass().add("hex-column");
            c.setSortable(false);
        });
        getColumns().get(0).getStyleClass().add("hex-address-column");
    }

    public void setBinary(OtpErlangBinary binary) {
        ByteArrayInputStream inputStream = new ByteArrayInputStream(binary.binaryValue());
        byte[] input = new byte[16];
        int addr = 0, bytesRead = 0;
        try {
            while ((bytesRead = inputStream.read(input)) > 0) {
                getItems().add(new DataRow(addr, input, bytesRead));
                addr += 16;
                Arrays.fill(input, (byte)0);
            }
        }
        catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private void column(String colName, String property) {
        TableColumn<DataRow, String> col;
        col = new TableColumn<DataRow,String>(colName);
        col.setCellValueFactory(new PropertyValueFactory<DataRow, String>(property));
        getColumns().add(col);
    }
}
