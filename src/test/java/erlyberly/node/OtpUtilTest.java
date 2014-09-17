package erlyberly.node;

import org.junit.Assert;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangBinary;


public class OtpUtilTest  {
	
	byte[] bytes;
	
	@Test
	public void binaryToString1() {
		bytes = "hello".getBytes();
		
		Assert.assertEquals("<<\"hello\">>", bin());
	}
	
	@Test
	public void binaryToString2() {
		bytes = "\0hello".getBytes();
		
		Assert.assertEquals("<<0, \"hello\">>", bin());
	}
	
	@Test
	public void binaryToString3() {
		bytes = "\0hello\0".getBytes();
		
		Assert.assertEquals("<<0, \"hello\", 0>>", bin());
	}
	
	@Test
	public void binaryToString4() {
		bytes = "_hello\0".getBytes();
		bytes[0] = 31;
		Assert.assertEquals("<<31, \"hello\", 0>>", bin());
	}
	
	private String bin() {
		OtpErlangBinary binary = new OtpErlangBinary(bytes);
		
		String string = OtpUtil.binaryToString(binary);
		
		return string;
	}
}
