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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;

import com.moandjiezana.toml.Toml;
import com.moandjiezana.toml.TomlWriter;

import erlyberly.ConnectionView.KnownNode;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;

/**
 * Load dot Properties files, bind them to JavaFX properties and auto-store
 * them when the JavaFX properties change.
 * <p>
 * Call {@link PrefBind#setup()} before hand then {@link PrefBind#bind(String, StringProperty)} away.
 * <p>
 * <b>All methods must be called on the JavaFX thread.</b>
 */
public class PrefBind {

    private static final long WRITE_TO_DISK_DELAY = 500L;

    private static final boolean IS_DAEMON = true;
    private static Timer timer = new Timer(IS_DAEMON);

    static {
        timer.scheduleAtFixedRate(new TimerTask() {
            @Override
            public void run() {
                synchronized (AWAIT_STORE_LOCK) {
                    if(!awaitingStore)
                        return;
                    awaitingStore = false;
                }
                store();
            }}, WRITE_TO_DISK_DELAY, WRITE_TO_DISK_DELAY);
    }

    private static Map<String, Object> props;

    private static File erlyberlyConfig;

    private static final Object AWAIT_STORE_LOCK = new Object();
    private static boolean awaitingStore;

    private PrefBind() {}

    public static void bind(final String propName, StringProperty stringProp) {
        if(props == null) {
            return;
        }
        String storedValue = (String) props.get(propName);
        if(storedValue != null) {
            stringProp.set(storedValue);
        }
        stringProp.addListener((ObservableValue<? extends String> o, String oldValue, String newValue) -> {
            set(propName, newValue);
        });
    }

    public static void bindBoolean(final String propName, BooleanProperty boolProp){
        if(props == null) {
            return;
        }
        Boolean storedValue = (Boolean) props.get(propName);
        if(storedValue != null) {
            boolProp.set(storedValue);
        }
        boolProp.addListener((ObservableValue<? extends Boolean> observable, Boolean oldValue, Boolean newValue) -> {
            set(propName, newValue);
        });
    }

    static void store() {
        try {
            //props.store(new FileOutputStream(erlyberlyConfig), " erlyberly at https://github.com/andytill/erlyberly");
            new TomlWriter().write(props, new FileOutputStream(erlyberlyConfig));
        }
        catch (IOException | NoClassDefFoundError e) {
            e.printStackTrace();
        }
    }

    public static void setup() throws IOException {
        String home = System.getProperty("user.home");

        File homeDir = new File(home);

        File dotConfigDir = new File(homeDir, ".config");

        if(dotConfigDir.exists()) {
            homeDir = dotConfigDir;
        }

        erlyberlyConfig = new File(homeDir, ".erlyberly2");
        erlyberlyConfig.createNewFile();

        Toml toml;
        toml = new Toml();
        toml.read(new FileInputStream(erlyberlyConfig));
        props = toml.getKeyValues();
    }

    public static Object get(Object key) {
        return props.get(key);
    }

    public static Object getOrDefault(String key, Object theDefault) {
        return props.getOrDefault(key, theDefault);
    }

    public static double getOrDefaultDouble(String key, Double theDefault) {
        return Double.parseDouble(PrefBind.getOrDefault(key, theDefault).toString());
    }

    public static boolean getOrDefaultBoolean(String key, boolean theDefault) {
        return Boolean.parseBoolean(PrefBind.getOrDefault(key, theDefault).toString());
    }

    public static void set(String propName, Object newValue) {
        props.put(propName, newValue);
        synchronized (AWAIT_STORE_LOCK) {
            awaitingStore = true;
        }
    }

    @SuppressWarnings("unchecked")
    public static List<List<String>> getKnownNodes() {
        return (List<List<String>>) props.getOrDefault("knownNodes", new ArrayList<>());
    }

    public static void storeKnownNode(KnownNode knownNode) {
        List<List<String>> knownNodes = getKnownNodes();
        knownNodes.add(Arrays.asList(knownNode.getNodeName(), knownNode.getCookie()));
        props.put("knownNodes", knownNodes);
        store();
    }

    public static void removeKnownNode(KnownNode knownNode) {
        List<String> nodeAsList = Arrays.asList(knownNode.getNodeName(), knownNode.getCookie());
        List<List<String>> knownNodes = getKnownNodes();
        knownNodes.remove(nodeAsList);
        props.put("knownNodes", knownNodes);
        store();
    }

    /**
     * The maximum number of traces that can be queued in the trace
     * proccesses queue before tracing is suspended on the node.
     */
    public static int getMaxTraceQueueLengthConfig() {
        Number maxTraceQueueLength = (Number)getOrDefault("maxTraceQueueLength", 1000);
        return maxTraceQueueLength.intValue();
    }
}
