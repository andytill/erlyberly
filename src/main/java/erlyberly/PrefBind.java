package erlyberly;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Properties;
import java.util.Timer;
import java.util.TimerTask;

import javafx.application.Platform;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;

/**
 * Load dot {@link Properties} files, bind them to JavaFX properties and auto-store
 * them when the JavaFX properties change.
 * <p>
 * Call {@link PrefBind#setup()} before hand then {@link PrefBind#bind(String, StringProperty)} away.
 * <p>
 * <b>All methods must be called on the JavaFX thread.</b>
 */
public class PrefBind {
	
	private static Timer timer = new Timer(true);
	
	private static Properties props;
	
	private static File erlyberlyConfig;
	
	private static boolean awaitingStore;

	public static void bind(final String propName, StringProperty stringProp) {
		if(props == null) {
			return;
		}
		
		String storedValue = props.getProperty(propName);
		
		if(storedValue != null) {
			stringProp.set(storedValue);
		}

		stringProp.addListener(new ChangeListener<String>() {
			@Override
			public void changed(ObservableValue<? extends String> o, String oldValue, String newValue) {
				props.setProperty(propName, newValue);
				
				timer.schedule(new TimerTask() {
					@Override
					public void run() {
						if(!awaitingStore) {
							Platform.runLater(PrefBind::store);
							awaitingStore = true;
						}
					}}, 1000);
			}});
	}
	
	static void store() {
		try {
			props.store(new FileOutputStream(erlyberlyConfig), " erlyberly at https://github.com/andytill/erlyberly");
		} catch (IOException e) {
			e.printStackTrace();
		}
		awaitingStore = false;
	}
	
	public static void setup() throws IOException {
		String home = System.getProperty("user.home");
		
		File homeDir = new File(home);
		
		File dotConfigDir = new File(homeDir, ".config");
		
		if(dotConfigDir.exists()) {
			homeDir = dotConfigDir;
		}
		
		erlyberlyConfig = new File(homeDir, ".erlyberly");
		erlyberlyConfig.createNewFile();
		
		Properties properties;
		
		properties = new Properties();
		properties.load(new FileInputStream(erlyberlyConfig));
		
		props = properties;
	}
	
	public static Object get(Object key) {
		return props.get(key);
	}

	public static Object getOrDefault(String key, Object theDefault) {
		return props.getOrDefault(key, theDefault);
	}
}
