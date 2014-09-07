import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;

import javafx.application.Application;
import javafx.application.Platform;
import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.beans.property.SimpleLongProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.Scene;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableColumn.SortType;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.stage.Stage;

import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpPeer;
import com.ericsson.otp.erlang.OtpSelf;


public class ErlyBerly extends Application {
	private final class ProcPollerThread extends Thread {
		public ObservableList<ProcInfo> processes2 = FXCollections.observableArrayList();

		public SimpleLongProperty updateCounter = new SimpleLongProperty();
		
		@Override
		public void run() {
			while(true) {
				try {
					Thread.sleep(1000);
				} catch (InterruptedException e1) {
					e1.printStackTrace();
				}
				
		    	final ArrayList<ProcInfo> processes = new ArrayList<>();
		    	
				try {
					retrieveProcessInfo(processes);
				} catch (Exception e) {
					e.printStackTrace();
				}
				
				Platform.runLater(new Runnable() {
					@Override
					public void run() {
						if(processView != null) {
							ObservableList<TableColumn<ProcInfo, ?>> sortOrder = processView.getSortOrder();
							
							if(sortOrder.size() > 0) {
								String id = sortOrder.get(0).getId();
								SortType sortType = sortOrder.get(0).getSortType();
								
								Comparator<ProcInfo> comparator = null;
								
								if("proc".equals(id)) {
									comparator = new Comparator<ProcInfo>() {
										@Override
										public int compare(ProcInfo o1, ProcInfo o2) {
											return o1.getProcessName().compareTo(o2.getProcessName());
										}};
								}
								else if("reduc".equals(id)) {
									comparator = new Comparator<ProcInfo>() {
										@Override
										public int compare(ProcInfo o1, ProcInfo o2) {
											return Long.compare(o1.getReductions(), o2.getReductions());
										}};
								}
								
								if(comparator != null) {
									if(sortType == SortType.DESCENDING) {
										comparator = Collections.reverseOrder(comparator);
									}
									Collections.sort(processes, comparator);
								}
							}
							
							processes2.clear();
							processes2.addAll(processes);
							
							updateCounter.add(1);
						}
					}});
			}
		}
	}

	public static void main(String[] args) throws Exception {
		launch(args);
	}

	private static HashMap<Object, Object> propsToMap(OtpErlangList pinfo) {
		HashMap<Object, Object> map = new HashMap<>();
		for (OtpErlangObject otpErlangObject : pinfo) {
			if(otpErlangObject instanceof OtpErlangTuple && ((OtpErlangTuple) otpErlangObject).arity() == 2) {
				OtpErlangTuple tuple = ((OtpErlangTuple) otpErlangObject);
				map.put(tuple.elementAt(0), tuple.elementAt(1));
			}
		}
		return map;
	}
	
	private OtpSelf self;
	
	private OtpConnection connection;
	
	private TableView<ProcInfo> processView;
	
	private ProcPollerThread procPollerThread;
	
    @Override
    public void start(Stage primaryStage) {
		try {
			self = new OtpSelf("erlyberly-" + System.currentTimeMillis());
			
			connection = self.connect(new OtpPeer("andye@CPADMIN-F4R0H8I"));
			
			procPollerThread = new ProcPollerThread();
			procPollerThread.start();
				
		} catch (Exception e) {
			e.printStackTrace();
		}
		
        primaryStage.setTitle(self.node());
        
        processView = new TableView<>();
        processView.setItems(procPollerThread.processes2);
        
        TableColumn<ProcInfo, String> processNameColumn = new TableColumn<>("Name");
        processNameColumn.setMinWidth(100);
        processNameColumn.setId("proc");
        processNameColumn.setCellValueFactory(new PropertyValueFactory<ProcInfo, String>("processName"));
        
        TableColumn<ProcInfo, String> reductionsColumn = new TableColumn<>("Reductions");
        reductionsColumn.setMinWidth(100);
        reductionsColumn.setId("reduc");
        reductionsColumn.setCellValueFactory(new PropertyValueFactory<ProcInfo, String>("reductions"));
 
        addColumns(processNameColumn, reductionsColumn);
        
        primaryStage.setScene(new Scene(processView, 300, 250));
        primaryStage.setTitle("erlyberly");
        primaryStage.show();
    }

	@SuppressWarnings("unchecked")
	private void addColumns(TableColumn<ProcInfo, String> processNameColumn,	TableColumn<ProcInfo, String> reductionsColumn) {
		processView.getColumns().addAll(processNameColumn, reductionsColumn);
	}

	private void retrieveProcessInfo(ArrayList<ProcInfo> processes) throws Exception {
		connection.sendRPC("erlang","processes",new OtpErlangList());
		OtpErlangList received = (OtpErlangList) connection.receiveRPC(); 
		
		for (OtpErlangObject pid : received) {
			connection.sendRPC("erlang","process_info",new OtpErlangList(pid));
			OtpErlangObject recv = connection.receiveRPC();
			
			if(recv instanceof OtpErlangList) {
				OtpErlangList pinfo = (OtpErlangList) recv; 
				HashMap<Object, Object> propsToMap = propsToMap(pinfo);
				propsToMap.put("pid", pid);
				processes.add(ProcInfo.toProcessInfo(propsToMap));
			}
		}
		
	}
}
