package erlyberly;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;

import javafx.application.Platform;
import javafx.beans.property.SimpleLongProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.TableColumn.SortType;

import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpPeer;
import com.ericsson.otp.erlang.OtpSelf;

/**
 * Logic and processing for the entop control. 
 */
public class ProcController {
	
	private final SimpleObjectProperty<ProcSort> procSortProperty;

	private OtpSelf self;
	
	private OtpConnection connection;
	
	private ProcPollerThread procPollerThread;
	
	public ProcController() {
		procSortProperty = new SimpleObjectProperty<>();
	}
	
	public void connect() {
		try {
			self = new OtpSelf("erlyberly-" + System.currentTimeMillis());
			
			connection = self.connect(new OtpPeer("andye@CPADMIN-F4R0H8I"));
			
			procPollerThread = new ProcPollerThread();
			procPollerThread.start();
				
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public ObservableList<ProcInfo> getProcs() {
		return procPollerThread.processes2;
	}
	
	public SimpleObjectProperty<ProcSort> procSortProperty() {
		return procSortProperty;
	}

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
						ProcSort procSort = procSortProperty.get();
						if(procSort != null) {
							Comparator<ProcInfo> comparator = null;
							
							if("proc".equals(procSort.getSortField())) {
								comparator = new Comparator<ProcInfo>() {
									@Override
									public int compare(ProcInfo o1, ProcInfo o2) {
										return o1.getProcessName().compareTo(o2.getProcessName());
									}};
							}
							else if("reduc".equals(procSort.getSortField())) {
								comparator = new Comparator<ProcInfo>() {
									@Override
									public int compare(ProcInfo o1, ProcInfo o2) {
										return Long.compare(o1.getReductions(), o2.getReductions());
									}};
							}
							
							if(comparator != null) {
								if(procSort.getSortType() == SortType.DESCENDING) {
									comparator = Collections.reverseOrder(comparator);
								}
								Collections.sort(processes, comparator);
							}
						}
						
						processes2.clear();
						processes2.addAll(processes);
						
						updateCounter.add(1);
					}});
			}
		}

		private void retrieveProcessInfo(ArrayList<ProcInfo> processes) throws Exception {
			connection.sendRPC("erlang", "processes", new OtpErlangList());
			OtpErlangList received = (OtpErlangList) connection.receiveRPC(); 
			
			for (OtpErlangObject pid : received) {
				connection.sendRPC("erlang", "process_info", new OtpErlangList(pid));
				OtpErlangObject recv = connection.receiveRPC();
				
				if(recv instanceof OtpErlangList) {
					OtpErlangList pinfo = (OtpErlangList) recv; 
					HashMap<Object, Object> propsToMap = OtpUtil.propsToMap(pinfo);
					propsToMap.put("pid", pid);
					processes.add(ProcInfo.toProcessInfo(propsToMap));
				}
			}
			
		}
	}
}
