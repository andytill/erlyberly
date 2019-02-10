package erlyberly;

import java.util.ArrayList;
import java.util.List;

import javafx.beans.InvalidationListener;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

/**
 * A list of erlang traces.
 */
public class TraceList {
    private final ObservableList<ModFunc> traces = FXCollections.observableArrayList();

    public boolean isTraces(ModFunc function) {
        return traces.contains(function);
    }

    public void remove(ModFunc mf) {
        traces.remove(mf);
    }

    public void add(ModFunc mf) {
        traces.add(mf);
    }

    public List<ModFunc> coptToList() {
        return new ArrayList<ModFunc>(traces);
    }

    public void addListener(InvalidationListener listener) {
        traces.addListener(listener);
    }
}
