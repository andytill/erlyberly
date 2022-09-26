/**
 * erlyberly, erlang trace debugger
 * Copyright (C) 2016 Andy Till
 * <p>
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * <p>
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * <p>
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package erlyberly.node;

import com.ericsson.otp.erlang.*;
import erlyberly.format.ErlangFormatter;
import erlyberly.format.LFEFormatter;
import org.hamcrest.CoreMatchers;
import org.hamcrest.MatcherAssert;
import org.junit.Test;

import java.nio.charset.StandardCharsets;


public class OtpUtilTest {

    private byte[] bytes;

    @Test
    public void binaryToString1() {
        this.bytes = "hello".getBytes(StandardCharsets.UTF_8);

        MatcherAssert.assertThat(this.bin(), CoreMatchers.is("<<\"hello\">>"));
    }

    @Test
    public void binaryToString2() {
        this.bytes = "\0hello".getBytes(StandardCharsets.UTF_8);

        MatcherAssert.assertThat(this.bin(), CoreMatchers.is("<<0, \"hello\">>"));
    }

    @Test
    public void binaryToString3() {
        this.bytes = "\0hello\0".getBytes(StandardCharsets.UTF_8);

        MatcherAssert.assertThat(this.bin(), CoreMatchers.is("<<0, \"hello\", 0>>"));
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
        final OtpErlangList improper = new OtpErlangList(new OtpErlangObject[]{new OtpErlangAtom("hello"), new OtpErlangBinary("x".getBytes(StandardCharsets.UTF_8)),}, new OtpErlangAtom("world"));
        MatcherAssert.assertThat(new ErlangFormatter().toString(improper), CoreMatchers.is("[hello, <<\"x\">>|world]"));
    }

    @Test
    public void improperListLFE() throws OtpErlangException {
        final OtpErlangList improper = new OtpErlangList(new OtpErlangObject[]{new OtpErlangAtom("hello"), new OtpErlangBinary("x".getBytes(StandardCharsets.UTF_8)),}, new OtpErlangAtom("world"));
        MatcherAssert.assertThat(new LFEFormatter().toString(improper), CoreMatchers.is("('hello, #B(\"x\").'world)"));
    }

    @Test
    public void bitstringErlang() {
        final OtpErlangBitstr bitstr = new OtpErlangBitstr(new byte[]{42, -100, 3 << 5}, 5);
        MatcherAssert.assertThat(new ErlangFormatter().toString(bitstr), CoreMatchers.is("<<42, 156, 3:3>>"));
    }

    @Test
    public void bitstringLFE() {
        final OtpErlangBitstr bitstr = new OtpErlangBitstr(new byte[]{42, -100, 3 << 5}, 5);
        MatcherAssert.assertThat(new LFEFormatter().toString(bitstr), CoreMatchers.is("#B(42 156 (3 (size 3)))"));
    }

    private String bin() {
        final OtpErlangBinary binary = new OtpErlangBinary(this.bytes);

        return new ErlangFormatter().toString(binary);
    }
}
