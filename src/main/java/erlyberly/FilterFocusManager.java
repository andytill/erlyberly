package erlyberly;

import java.util.ArrayList;

import javafx.application.Platform;
import javafx.scene.Scene;
import javafx.scene.control.Control;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyCodeCombination;
import javafx.scene.input.KeyCombination;

public class FilterFocusManager {

    private static final KeyCodeCombination REFRESH_MODULES_SHORTCUT = new KeyCodeCombination(KeyCode.F, KeyCombination.SHORTCUT_DOWN);

    private static final ArrayList<Control> filters = new ArrayList<>();

    private static int lastFocusedIndex = -1;

    public static void init(Scene scene) {
        filters.add(null);
        filters.add(null);
        filters.add(null);
        
        Platform.runLater(() -> {
            scene.getAccelerators().put(REFRESH_MODULES_SHORTCUT, () -> { nextFilter(); });
        });
    }
    
    private static void nextFilter() {
        if(filters.isEmpty())
            return;
        
        Control control = findNextFilter();
        
        requestFocus(control);
    }

    private static Control findNextFilter() {
        if(lastFocusedIndex != -1) {
            Control lastFocused = filters.get(lastFocusedIndex);
            if(!lastFocused.isFocused()) {
                return lastFocused;
            }
        }
        
        int focused = findCurrentFilter();
        
        if(focused == -1) {
            focused = 0;
        }
        else {
            // iterate the filters until we find one that is part of the scene or we run out of filters
            int iterations = 0;
            while(iterations < filters.size()) {
                iterations++;
                focused++;
                if(focused >= filters.size()) {
                    focused = 0;
                }
                boolean isFocusable = filters.get(focused).getScene() != null;
                if(isFocusable)
                    break;
            }
        }
        
        return filters.get(focused);
    }

    private static int findCurrentFilter() {
        int focused = -1;
        for (int i = 0; i < filters.size(); i++) {
            Control control = filters.get(i);
            if(control != null && control.isFocused()) {
                focused = i;
                break;
            }
        }
        return focused;
    }

   private static void requestFocus(Control control) {
       if(control != null)
           Platform.runLater(() -> { control.requestFocus(); });
    }

   public static void addFilter(Control control, int order) {
        assert control != null;
        assert order >= 0;
        assert Platform.isFxApplicationThread();
        
        filters.set(order, control);
        
        control.focusedProperty().addListener((o, oldFocus, newFocus) -> {
            lastFocusedIndex = order;
        });
    }
}
