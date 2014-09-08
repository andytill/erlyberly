package erlyberly;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;

import javafx.beans.property.SimpleBooleanProperty;

import com.ericsson.otp.erlang.OtpAuthException;
import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpPeer;
import com.ericsson.otp.erlang.OtpSelf;

public class NodeAPI {
	
	private static final String ERLYBERLY_BEAM_PATH = "/erlyberly/beam/erlyberly.beam";
	
	private static final int BEAM_SIZE_LIMIT = 1024 * 20;
	
	private final SimpleBooleanProperty connectedProperty;
	
	private OtpConnection connection;

	private OtpSelf self;
	
	public NodeAPI() {
		connectedProperty = new SimpleBooleanProperty();
	}
	
	public void connect(String remoteNodeName) {
		String nodeName = remoteNodeName;

		try {
			self = new OtpSelf("erlyberly-" + System.currentTimeMillis());
			
			// if the node name does not contain a host then assume it is on the
			// same machine
			if(!nodeName.contains("@")) {
				String[] split = self.toString().split("\\@");
				
				nodeName += "@" + split[1];
			}
			
			connection = self.connect(new OtpPeer(nodeName));
			
			loadRemoteErlyberly();
			
			connectedProperty.set(true);
		} catch (Exception e) {
			throw new RuntimeException("Error connecting to remote node " + nodeName, e);
		}
	}
	
	private void loadRemoteErlyberly() throws IOException, OtpErlangExit, OtpAuthException {
		OtpErlangObject[] elems = new OtpErlangObject[]{
				new OtpErlangAtom("erlyberly"),
				new OtpErlangString(ERLYBERLY_BEAM_PATH),
				new OtpErlangBinary(loadBeamFile())
		};
		
		connection.sendRPC("code", "load_binary", new OtpErlangList(elems));
		
		OtpErlangObject receiveRPC = connection.receiveRPC();
		
		System.out.println(receiveRPC);
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

	public void retrieveProcessInfo(ArrayList<ProcInfo> processes) throws Exception {
		connection.sendRPC("erlyberly", "process_info", new OtpErlangList());
		OtpErlangList received = (OtpErlangList) connection.receiveRPC(); 
		
		for (OtpErlangObject recv : received) {
			if(recv instanceof OtpErlangList) {
				OtpErlangList pinfo = (OtpErlangList) recv; 
				HashMap<Object, Object> propsToMap = OtpUtil.propsToMap(pinfo);
				processes.add(ProcInfo.toProcessInfo(propsToMap));
			}
		}
	}

	public SimpleBooleanProperty connectedProperty() {
		return connectedProperty;
	}
}
