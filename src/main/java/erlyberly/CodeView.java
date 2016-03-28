package erlyberly;

import javafx.scene.control.TextArea;

public class CodeView extends TextArea {

    {
        getStyleClass().add("mod-src");
        setEditable(false);
    }

    public CodeView() {
        super();
    }

    public CodeView(String text) {
        super(text);
    }

}
