package erlyberly.node;

import static erlyberly.node.OtpUtil.*;

import java.io.IOException;
import java.io.InputStream;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.concurrent.atomic.AtomicBoolean;

import javafx.application.Platform;
import javafx.beans.Observable;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;

import com.ericsson.otp.erlang.OtpAuthException;
import com.ericsson.otp.erlang.OtpConn;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpPeer;
import com.ericsson.otp.erlang.OtpSelfNode;

import erlyberly.ModFunc;
import erlyberly.ProcInfo;
import erlyberly.SeqTraceLog;
import erlyberly.TraceLog;

public class NodeAPI {
	
	public interface RpcCallback<T> {
		void callback(T result);
	}
	
	private static final OtpErlangAtom MODULE_ATOM = new OtpErlangAtom("module");

	private static final String ERLYBERLY_BEAM_PATH = "/erlyberly/beam/erlyberly.beam";
	
	private static final int BEAM_SIZE_LIMIT = 1024 * 20;
	
	private final TraceManager traceManager;
	
	private final SimpleBooleanProperty connectedProperty;
	
	private final SimpleStringProperty summary;
	
	private OtpConn connection;

	private OtpSelfNode self;

	private String remoteNodeName;

	private String cookie;

	private volatile Thread checkAliveThread;
	
	private final SimpleObjectProperty<AppProcs> appProcs;

	private OtpMbox mbox;
	
	private final AtomicBoolean connected = new AtomicBoolean();
	
	public NodeAPI() {
		traceManager = new TraceManager();
		
		connectedProperty = new SimpleBooleanProperty();
		connectedProperty.addListener(new ChangeListener<Boolean>() {
			@Override
			public void changed(ObservableValue<? extends Boolean> obv, Boolean o, Boolean n) {
				connected.set(n);
			}});
		summary = new SimpleStringProperty("erlyberly not connected");
		
		appProcs = new SimpleObjectProperty<AppProcs>(new AppProcs(0, LocalDateTime.now()));
		
		connectedProperty.addListener(this::summaryUpdater);
	}
	
	public NodeAPI connectionInfo(String remoteNodeName, String cookie) {
		this.remoteNodeName = remoteNodeName;
		this.cookie = cookie;
		
		return this;
	}
	
	public SimpleObjectProperty<AppProcs> appProcsProperty() {
		return appProcs;
	}

	public synchronized void connect() throws IOException, OtpErlangException, OtpAuthException {
		assert !Platform.isFxApplicationThread() : "cannot run this method from the FX thread";
		
		self = new OtpSelfNode("erlyberly-" + System.currentTimeMillis());
		if(!cookie.isEmpty()) {
			self.setCookie(cookie);
		}
		
		// if the node name does not contain a host then assume it is on the
		// same machine
		if(!remoteNodeName.contains("@")) {
			String[] split = self.toString().split("\\@");
			
			remoteNodeName += "@" + split[1];
		}
		
		connection = self.connect(new OtpPeer(remoteNodeName));
		
		mbox = self.createMbox();
		
		loadRemoteErlyberly();

		Platform.runLater(() -> { connectedProperty.set(true); });
		
		if(checkAliveThread != null)
			return;
		
		checkAliveThread = new CheckAliveThread();
		checkAliveThread.start();
	}
	
	class CheckAliveThread extends Thread {
		public CheckAliveThread() {
			setDaemon(true);
			setName("Erlyberly Check Alive");
		}
		
		@Override
		public void run() {
			while(true) {
				ensureAlive();
				
				mySleep(450);
			}
		}
	}
	
	private void loadRemoteErlyberly() throws IOException, OtpErlangException {
		
		sendRPC("code", "load_binary", 
				list(
						atom("erlyberly"),
						new OtpErlangString(ERLYBERLY_BEAM_PATH),
						new OtpErlangBinary(loadBeamFile())));
		
		OtpErlangObject result = receiveRPC();
		
		if(result instanceof OtpErlangTuple) {
			OtpErlangObject e0 = ((OtpErlangTuple) result).elementAt(0);
			
			if(!MODULE_ATOM.equals(e0)) {
				throw new RuntimeException("error loading the erlyberly module, result was " + result);
			}
		}
		else {
			throw new RuntimeException("error loading the erlyberly module, result was " + result);
		}
	}

	private OtpErlangObject receiveRPC() throws IOException, OtpErlangException {
		OtpErlangObject result = OtpUtil.receiveRPC(mbox);
		
		// hack to support certain projects, don't ask...
		if(result instanceof OtpErlangTuple) {
			if(new OtpErlangAtom("add_locator").equals(((OtpErlangTuple) result).elementAt(0))) {
				result = receiveRPC();
			}
		}
		
		return result;
	}

	private static byte[] loadBeamFile() throws IOException {
		InputStream resourceAsStream = OtpUtil.class.getResourceAsStream(ERLYBERLY_BEAM_PATH);
		byte[] b = new byte[BEAM_SIZE_LIMIT];
		int read = resourceAsStream.read(b);
		
		if(read >= BEAM_SIZE_LIMIT) {
			throw new RuntimeException("erlyberly.beam file is too big");
		}
		
		return b;
	}

	public synchronized void retrieveProcessInfo(ArrayList<ProcInfo> processes) throws Exception {
		assert !Platform.isFxApplicationThread() : "cannot run this method from the FX thread";
		
		OtpErlangObject receiveRPC = null;
		
		try {
			sendRPC("erlyberly", "process_info", new OtpErlangList());
			receiveRPC = receiveRPC();
			OtpErlangList received = (OtpErlangList) receiveRPC; 
			
			for (OtpErlangObject recv : received) {
				if(recv instanceof OtpErlangList) {
					OtpErlangList pinfo = (OtpErlangList) recv; 
					HashMap<Object, Object> propsToMap = OtpUtil.propsToMap(pinfo);
					processes.add(ProcInfo.toProcessInfo(propsToMap));
				}
			}
			
			Platform.runLater(() -> { appProcs.set(new AppProcs(processes.size(), LocalDateTime.now())); });
		} catch (ClassCastException e) {
			throw new RuntimeException("unexpected result: " + receiveRPC, e);
		}
	}

	private boolean ensureAlive() {
		if(connection.isAlive())
			return true;
		
		Platform.runLater(() -> { connectedProperty.set(false); });
		
		while(true) {
			try {
				connect();
				break;
			}
			catch(Exception e) {
				int millis = 50;
				mySleep(millis);
				
			}
		}
		return true;
	}

	private void mySleep(int millis) {
		try {
			Thread.sleep(millis);
		} catch (InterruptedException e1) {
			e1.printStackTrace();
		}
	}

	public synchronized OtpErlangList requestFunctions() throws Exception {
		assert !Platform.isFxApplicationThread() : "cannot run this method from the FX thread";
		
		sendRPC("erlyberly", "module_functions", new OtpErlangList());
		return (OtpErlangList) receiveRPC(); 
	}

	public synchronized void startTrace(ModFunc mf) throws Exception {
		assert mf.getFuncName() != null : "function name cannot be null";
		
		sendRPC("erlyberly", "start_trace", toTraceTuple(mf));
		
		OtpErlangObject result = receiveRPC();
		
		if(isTupleTagged(atom("error"), result)) {
			System.out.println(result);
		
			
			// TODO notify caller of failure!
			return;
		}
	}

	public synchronized void stopTrace(ModFunc mf) throws Exception {
		assert mf.getFuncName() != null : "function name cannot be null";

		sendRPC("erlyberly", "stop_trace", 
			list(
				OtpUtil.atom(mf.getModuleName()),
				OtpUtil.atom(mf.getFuncName()),
				new OtpErlangInt(mf.getArity()),
				new OtpErlangAtom(mf.isExported())
			));
		receiveRPC();
	}

	private OtpErlangList toTraceTuple(ModFunc mf) {
		return list(
			tuple(OtpUtil.atom(self.node()), mbox.self()),
			atom(mf.getModuleName()),
			atom(mf.getFuncName()),
			mf.getArity(),
			mf.isExported()
		);
	}

	public SimpleBooleanProperty connectedProperty() {
		return connectedProperty;
	}
	
	private void sendRPC(String module, String function, OtpErlangList args) throws IOException {
		OtpUtil.sendRPC(connection, mbox, atom(module), atom(function), args);
	}

	public synchronized ArrayList<TraceLog> collectTraceLogs() throws Exception {
		sendRPC("erlyberly", "collect_trace_logs", new OtpErlangList());
		
		OtpErlangObject prcResult = receiveRPC();
		
		if(!isTupleTagged(OK_ATOM, prcResult)) {
			System.out.println(prcResult);
			
			return new ArrayList<TraceLog>();
		}
		
		OtpErlangList traceLogs = (OtpErlangList) ((OtpErlangTuple) prcResult).elementAt(1);
		
		return traceManager.collateTraces(traceLogs);
	}

	public synchronized ArrayList<SeqTraceLog> collectSeqTraceLogs() throws Exception {
		sendRPC("erlyberly", "collect_seq_trace_logs", new OtpErlangList());
		
		OtpErlangObject prcResult = receiveRPC();
		
		if(!isTupleTagged(OK_ATOM, prcResult)) {
			return new ArrayList<SeqTraceLog>();
		}
		
		ArrayList<SeqTraceLog> seqLogs = new ArrayList<SeqTraceLog>();
		
		try {
			OtpErlangList traceLogs = (OtpErlangList) ((OtpErlangTuple) prcResult)
					.elementAt(1);
			for (OtpErlangObject otpErlangObject : traceLogs) {
				seqLogs.add(SeqTraceLog.build(OtpUtil
						.propsToMap((OtpErlangList) otpErlangObject)));
			}
		}
		catch (ClassCastException e) {
			System.out.println("did not understand result from collect_seq_trace_logs " + prcResult);
			e.printStackTrace();
		}
		return seqLogs;
	}

	public ObservableValue<? extends String> summaryProperty() {
		return summary;
	}
	
	private void summaryUpdater(Observable o, Boolean wasConnected, Boolean isConnected) {
		String summaryText = "erlyberly";
		
		if(!wasConnected && isConnected)
			summaryText = "erlyberly, connected as " + self.node();
		else if(wasConnected && !isConnected)
			summaryText = "erlyberly, connection lost.  reconnecting..."; 
		
		summary.set(summaryText);
	}

	public synchronized void seqTrace(ModFunc mf) throws IOException, OtpErlangException {
		sendRPC("erlyberly", "seq_trace", 
			list(
				tuple(OtpUtil.atom(self.node()), mbox.self()),
				atom(mf.getModuleName()),
				atom(mf.getFuncName()),
				mf.getArity(),
				new OtpErlangAtom(mf.isExported())
			));
		
		OtpErlangObject result = receiveRPC();
		
		System.out.println(result);
	}

	public synchronized OtpErlangObject getProcessState(String pidString) throws IOException, OtpErlangException {
		sendRPC("erlyberly", "get_process_state", list(pidString));
		
		OtpErlangObject result = receiveRPC();
		
		if(isTupleTagged(OK_ATOM, result)) {
			return ((OtpErlangTuple)result).elementAt(1);
		}
		return null;
	}

	public synchronized HashMap<Object, Object> erlangMemory() throws IOException, OtpErlangException {
		sendRPC("erlang", "memory", list());
		
		OtpErlangList result = (OtpErlangList) receiveRPC();
		
		return OtpUtil.propsToMap(result);
	}

	public boolean isConnected() {
		return connected.get();
	}
}
