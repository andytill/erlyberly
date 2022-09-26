package hextstar;

import com.ericsson.otp.erlang.OtpErlangBinary;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.PropertyValueFactory;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Arrays;

/**
 * Created by Bart on 9/26/2015.
 * Originally from <a href="https://github.com/Velocity-/Hexstar">...</a>
 */
public class HexstarView extends TableView<DataRow> {

    public HexstarView() {
        super();
        //setContextMenu(new ContextMenu(new MenuItem("Copy"), new MenuItem("Cut"), new MenuItem("Delete")));
        this.getSelectionModel().setCellSelectionEnabled(true);
        this.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);

        this.column("Address", "address");
        this.column("0", "b0");
        this.column("1", "b1");
        this.column("2", "b2");
        this.column("3", "b3");
        this.column("4", "b4");
        this.column("5", "b5");
        this.column("6", "b6");
        this.column("7", "b7");
        this.column("8", "b8");
        this.column("9", "b9");
        this.column("A", "bA");
        this.column("B", "bB");
        this.column("C", "bC");
        this.column("D", "bD");
        this.column("E", "bE");
        this.column("F", "bF");
        this.column("Text", "text");

        this.getColumns().forEach(c -> {
            c.getStyleClass().add("hex-column");
            c.setSortable(false);
        });
        this.getColumns().get(0).getStyleClass().add("hex-address-column");
    }

    public void setBinary(final OtpErlangBinary binary) {
        final ByteArrayInputStream inputStream = new ByteArrayInputStream(binary.binaryValue());
        final byte[] input = new byte[16];
        int addr = 0, bytesRead;
        try {
            while (0 < (bytesRead = inputStream.read(input))) {
                this.getItems().add(new DataRow(addr, input, bytesRead));
                addr += 16;
                Arrays.fill(input, (byte) 0);
            }
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
    }

    private void column(final String colName, final String property) {
        final TableColumn<DataRow, String> col;
        col = new TableColumn<>(colName);
        col.setCellValueFactory(new PropertyValueFactory<>(property));
        this.getColumns().add(col);
    }
}
