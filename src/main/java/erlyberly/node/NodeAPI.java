package erlyberly.node;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;

import javafx.application.Platform;
import javafx.beans.property.SimpleBooleanProperty;

import com.ericsson.otp.erlang.OtpAuthException;
import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpPeer;
import com.ericsson.otp.erlang.OtpSelf;

import erlyberly.ModFunc;
import erlyberly.ProcInfo;

import static erlyberly.node.OtpUtil.*;

public class NodeAPI {
	
	private static final OtpErlangAtom MODULE_ATOM = new OtpErlangAtom("module");

	private static final String ERLYBERLY_BEAM_PATH = "/erlyberly/beam/erlyberly.beam";
	
	private static final int BEAM_SIZE_LIMIT = 1024 * 20;
	
	private final SimpleBooleanProperty connectedProperty;
	
	private OtpConnection connection;

	private OtpSelf self;

	private String remoteNodeName;

	private String cookie;
	
	public NodeAPI() {
		connectedProperty = new SimpleBooleanProperty();
	}
	
	public NodeAPI connectionInfo(String remoteNodeName, String cookie) {
		this.remoteNodeName = remoteNodeName;
		this.cookie = cookie;
		
		return this;
	}
	
	public synchronized void connect() throws IOException, OtpAuthException, OtpErlangExit {
		
		String nodeName = remoteNodeName;
		self = new OtpSelf("erlyberly-" + System.currentTimeMillis());
		
		if(!cookie.isEmpty()) {
			self.setCookie(cookie);
		}
		
		// if the node name does not contain a host then assume it is on the
		// same machine
		if(!nodeName.contains("@")) {
			String[] split = self.toString().split("\\@");
			
			nodeName += "@" + split[1];
		}
		
		connection = self.connect(new OtpPeer(nodeName));
		
		loadRemoteErlyberly();

		Platform.runLater(() -> { connectedProperty.set(true); });
	}
	
	private void loadRemoteErlyberly() throws IOException, OtpErlangExit, OtpAuthException {
		OtpErlangObject[] elems = new OtpErlangObject[]{
				new OtpErlangAtom("erlyberly"),
				new OtpErlangString(ERLYBERLY_BEAM_PATH),
				new OtpErlangBinary(loadBeamFile())
		};
		
		connection.sendRPC("code", "load_binary", new OtpErlangList(elems));
		
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

	private OtpErlangObject receiveRPC() throws IOException, OtpErlangExit, OtpAuthException {
		OtpErlangObject result = connection.receiveRPC();
		
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
		ensureAlive();
		OtpErlangObject receiveRPC = null;
		
		try {
			connection.sendRPC("erlyberly", "process_info", new OtpErlangList());
			receiveRPC = receiveRPC();
			OtpErlangList received = (OtpErlangList) receiveRPC; 
			
			for (OtpErlangObject recv : received) {
				if(recv instanceof OtpErlangList) {
					OtpErlangList pinfo = (OtpErlangList) recv; 
					HashMap<Object, Object> propsToMap = OtpUtil.propsToMap(pinfo);
					processes.add(ProcInfo.toProcessInfo(propsToMap));
				}
			}
		} catch (ClassCastException e) {
			throw new RuntimeException("unexpected result: " + receiveRPC, e);
		}
	}

	private void ensureAlive() {
		if(connection.isAlive())
			return;
		
		Platform.runLater(() -> { connectedProperty.set(false); });
		
		while(true) {
			try {
				connect();
				break;
			}
			catch(Exception e) {
				try {
					Thread.sleep(1000);
				} catch (InterruptedException e1) {
					e1.printStackTrace();
				}
				System.out.println("couldn't connect: " + e.getMessage());
				
			}
		}
	}

	public synchronized OtpErlangList requestFunctions() throws Exception {
		ensureAlive();
		connection.sendRPC("erlyberly", "module_functions", new OtpErlangList());
		return (OtpErlangList) receiveRPC(); 
	}

	public synchronized void startTrace(ModFunc mf) throws Exception {
		assert mf.getFuncName() != null : "function name cannot be null";
		
		ensureAlive();
		
		connection.sendRPC("erlyberly", "start_trace", toTraceTuple(mf));
		
		OtpErlangObject result = receiveRPC();
		
		if(!isTupleTagged(OK_ATOM, result)) {
			System.out.println(result);
		}
	}

	public synchronized void stopTrace(ModFunc mf) throws Exception {
		ensureAlive();
		assert mf.getFuncName() != null : "function name cannot be null";
		
		connection.sendRPC("erlyberly", "stop_trace", new OtpErlangObject[] {
				OtpUtil.atom(mf.getModuleName()),
				OtpUtil.atom(mf.getFuncName()),
				new OtpErlangInt(mf.getArity()),
				new OtpErlangAtom(mf.isExported())
			});
		receiveRPC();
	}

	private OtpErlangObject[] toTraceTuple(ModFunc mf) {
		OtpErlangObject[] args = new OtpErlangObject[] {
			OtpUtil.tuple(OtpUtil.atom(self.node()), self.pid()),
			OtpUtil.atom(mf.getModuleName()),
			OtpUtil.atom(mf.getFuncName()),
			new OtpErlangInt(mf.getArity()),
			new OtpErlangAtom(mf.isExported())
		};
		return args;
	}

	public SimpleBooleanProperty connectedProperty() {
		return connectedProperty;
	}

	public ArrayList<OtpErlangObject> collectTraceLogs() throws Exception {
		ensureAlive();
		
		connection.sendRPC("erlyberly", "collect_trace_logs", new OtpErlangList());
		
		ArrayList<OtpErlangObject> traceList = new ArrayList<OtpErlangObject>();
		
		OtpErlangObject prcResult = receiveRPC();
		
		if(!isTupleTagged(OK_ATOM, prcResult)) {
			System.out.println(prcResult);
			
			return traceList;
		}
		
		OtpErlangList traceLogs = (OtpErlangList) ((OtpErlangTuple) prcResult).elementAt(1);
		
		for (OtpErlangObject otpErlangObject : traceLogs) {
			traceList.add(otpErlangObject);
		}
		return traceList;
	}
}
