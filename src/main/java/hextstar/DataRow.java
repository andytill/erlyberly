package hextstar;

import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;

/**
 * Created by Bart on 9/26/2015.
 * Originally from https://github.com/Velocity-/Hexstar
 */
public class DataRow {
    private final StringProperty address;
    private final StringProperty text;
    private final StringProperty[] data = new StringProperty[16];

    public DataRow(int addr, byte[] b, int bytesRead) {
        address = new SimpleStringProperty(String.format("%08X", addr));
        text = new SimpleStringProperty();
        StringBuilder sb = new StringBuilder(bytesRead);
        for (int i = 0; i < b.length; i++) {
            String stringByteValue = "";
            // display the byte as an unsigned number, like erlang binaries
            if (i < bytesRead)
                stringByteValue = Integer.toString((b[i] & 0xFF));
            data[i] = new SimpleStringProperty(stringByteValue);
            if (b[i] == 10)
                sb.append("\\n");
            else if (b[i] >= 32 && b[i] < 127)
                sb.append((char) b[i]);
            else
                sb.append(".");
        }
        text.set(sb.toString());
    }

    public StringProperty addressProperty() {
        return address;
    }

    public StringProperty textProperty() {
        return text;
    }

    public StringProperty b0Property() {
        return data[0];
    }

    public StringProperty b1Property() {
        return data[1];
    }

    public StringProperty b2Property() {
        return data[2];
    }

    public StringProperty b3Property() {
        return data[3];
    }

    public StringProperty b4Property() {
        return data[4];
    }

    public StringProperty b5Property() {
        return data[5];
    }

    public StringProperty b6Property() {
        return data[6];
    }

    public StringProperty b7Property() {
        return data[7];
    }

    public StringProperty b8Property() {
        return data[8];
    }

    public StringProperty b9Property() {
        return data[9];
    }

    public StringProperty bAProperty() {
        return data[0xA];
    }

    public StringProperty bBProperty() {
        return data[0xB];
    }

    public StringProperty bCProperty() {
        return data[0xC];
    }

    public StringProperty bDProperty() {
        return data[0xD];
    }

    public StringProperty bEProperty() {
        return data[0xE];
    }

    public StringProperty bFProperty() {
        return data[0xF];
    }
}

