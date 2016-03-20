package erlyberly.node;

import org.junit.Assert;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangBinary;

import erlyberly.format.ErlangFormatter;


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
	/*
	@Test
	public void binaryToString4() {
		bytes = "_hello\0".getBytes();
		bytes[0] = 31;
		Assert.assertEquals("<<31, \"hello\", 0>>", bin());
	}
	
	@Test
	public void otpObjectToStringList0() {
		OtpErlangList list = OtpUtil.list();
		StringBuilder sb = new StringBuilder();;
		OtpUtil.otpObjectToString(list, sb );
		Assert.assertEquals("[]", sb.toString());
	}
	
	@Test
	public void otpObjectToStringList1() {
		OtpErlangList list = OtpUtil.list(1);
		StringBuilder sb = new StringBuilder();;
		OtpUtil.otpObjectToString(list, sb );
		Assert.assertEquals("[1]", sb.toString());
	}
	
	@Test
	public void otpObjectToStringList2() {
		OtpErlangList list = OtpUtil.list(1,2);
		StringBuilder sb = new StringBuilder();;
		OtpUtil.otpObjectToString(list, sb );
		Assert.assertEquals("[1, 2]", sb.toString());
	}
	
	@Test
	public void otpObjectToStringTuple0() {
		OtpErlangTuple tuple = OtpUtil.tuple();
		StringBuilder sb = new StringBuilder();;
		OtpUtil.otpObjectToString(tuple, sb );
		Assert.assertEquals("{}", sb.toString());
	}
	
	@Test
	public void otpObjectToStringTuple1() {
		OtpErlangTuple tuple = OtpUtil.tuple(1);
		StringBuilder sb = new StringBuilder();;
		OtpUtil.otpObjectToString(tuple, sb );
		Assert.assertEquals("{1}", sb.toString());
	}
	
	@Test
	public void otpObjectToStringTuple2() {
		OtpErlangTuple tuple = OtpUtil.tuple(1,2);
		StringBuilder sb = new StringBuilder();;
		OtpUtil.otpObjectToString(tuple, sb );
		Assert.assertEquals("{1, 2}", sb.toString());
	}
	
	@Test
	public void otpObjectToStringTuple3() {
		OtpErlangTuple tuple = OtpUtil.tuple(1,2,3);
		StringBuilder sb = new StringBuilder();;
		OtpUtil.otpObjectToString(tuple, sb );
		Assert.assertEquals("{1, 2, 3}", sb.toString());
	}
	
	@Test
	public void otpObjectToStringNestedTuples() {
		OtpErlangTuple tuple = OtpUtil.tuple(1,2, OtpUtil.tuple(3,4));
		StringBuilder sb = new StringBuilder();;
		OtpUtil.otpObjectToString(tuple, sb );
		Assert.assertEquals("{1, 2, {3, 4}}", sb.toString());
	}
	*/
	private String bin() {
		OtpErlangBinary binary = new OtpErlangBinary(bytes);
		
		return new ErlangFormatter().toString(binary);
	}
}
