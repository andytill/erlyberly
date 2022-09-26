package hextstar;

import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;

/**
 * Created by Bart on 9/26/2015.
 * Originally from <a href="https://github.com/Velocity-/Hexstar">...</a>
 */
public class DataRow {
    private final StringProperty address;
    private final StringProperty text;
    private final StringProperty[] data = new StringProperty[16];

    DataRow(final int addr, final byte[] b, final int bytesRead) {
        super();
        this.address = new SimpleStringProperty(String.format("%08X", addr));
        this.text = new SimpleStringProperty();
        final StringBuilder sb = new StringBuilder(bytesRead);
        for (int i = 0; i < b.length; i++) {
            String stringByteValue = "";
            // display the byte as an unsigned number, like erlang binaries
            if (i < bytesRead)
                stringByteValue = Integer.toString((b[i] & 0xFF));
            this.data[i] = new SimpleStringProperty(stringByteValue);
            if (10 == b[i])
                sb.append("\\n");
            else if (32 <= b[i] && 127 > b[i])
                sb.append((char) b[i]);
            else
                sb.append(".");
        }
        this.text.set(sb.toString());
    }

    public StringProperty addressProperty() {
        return this.address;
    }

    public StringProperty textProperty() {
        return this.text;
    }

    public StringProperty b0Property() {
        return this.data[0];
    }

    public StringProperty b1Property() {
        return this.data[1];
    }

    public StringProperty b2Property() {
        return this.data[2];
    }

    public StringProperty b3Property() {
        return this.data[3];
    }

    public StringProperty b4Property() {
        return this.data[4];
    }

    public StringProperty b5Property() {
        return this.data[5];
    }

    public StringProperty b6Property() {
        return this.data[6];
    }

    public StringProperty b7Property() {
        return this.data[7];
    }

    public StringProperty b8Property() {
        return this.data[8];
    }

    public StringProperty b9Property() {
        return this.data[9];
    }

    public StringProperty bAProperty() {
        return this.data[0xA];
    }

    public StringProperty bBProperty() {
        return this.data[0xB];
    }

    public StringProperty bCProperty() {
        return this.data[0xC];
    }

    public StringProperty bDProperty() {
        return this.data[0xD];
    }

    public StringProperty bEProperty() {
        return this.data[0xE];
    }

    public StringProperty bFProperty() {
        return this.data[0xF];
    }
}

