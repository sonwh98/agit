import java.math.BigInteger;
import java.security.MessageDigest;

public class Sha1{
    public static void main(String[] args) throws Exception{
        MessageDigest md = MessageDigest.getInstance("SHA-1");
        String data = "foobar\n";
        String gitData = "blob "+data.length()+"\0"+data;
        byte[] shaBytes = md.digest(gitData.getBytes()); 
        String sha1 = new BigInteger(1, shaBytes).toString(16);
        System.out.println(sha1);
    }
}
