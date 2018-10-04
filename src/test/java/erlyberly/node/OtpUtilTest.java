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
package erlyberly.node;

import org.junit.Assert;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangBitstr;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

import erlyberly.format.ErlangFormatter;
import erlyberly.format.LFEFormatter;


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

    @Test
    public void improperListErlang() throws OtpErlangException {
        OtpErlangList improper =
            new OtpErlangList(
                    new OtpErlangObject[] {
                        new OtpErlangAtom("hello"),
                        new OtpErlangBinary("x".getBytes()),
                    },
                    new OtpErlangAtom("world"));
        Assert.assertEquals(
                "[hello, <<\"x\">>|world]",
                new ErlangFormatter().toString(improper));
    }

    @Test
    public void improperListLFE() throws OtpErlangException {
        OtpErlangList improper =
            new OtpErlangList(
                    new OtpErlangObject[] {
                        new OtpErlangAtom("hello"),
                        new OtpErlangBinary("x".getBytes()),
                    },
                    new OtpErlangAtom("world"));
        Assert.assertEquals(
                "('hello, #B(\"x\").'world)",
                new LFEFormatter().toString(improper));
    }

    @Test
    public void bitstringErlang() throws OtpErlangException {
        OtpErlangBitstr bitstr = new OtpErlangBitstr(new byte[]{42, -100, 3 << 5}, 5);
        Assert.assertEquals(
                "<<42, 156, 3:3>>",
                new ErlangFormatter().toString(bitstr));
    }

    @Test
    public void bitstringLFE() throws OtpErlangException {
        OtpErlangBitstr bitstr = new OtpErlangBitstr(new byte[]{42, -100, 3 << 5}, 5);
        Assert.assertEquals(
                "#B(42 156 (3 (size 3)))",
                new LFEFormatter().toString(bitstr));
    }

	private String bin() {
		OtpErlangBinary binary = new OtpErlangBinary(bytes);
		
		return new ErlangFormatter().toString(binary);
	}
}
