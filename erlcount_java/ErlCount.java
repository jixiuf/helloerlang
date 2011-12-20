
import java.util.Iterator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.IOException;
import java.util.Stack;
import java.util.ArrayList;
import java.util.List;
import java.io.FileFilter;
import java.io.File;

public class ErlCount {
    public String read(File f ){
        StringBuffer strb = new StringBuffer();
        BufferedReader reader = null;
        try {
            reader = new BufferedReader( new InputStreamReader(new FileInputStream(f), "UTF-8"));
            while (true) {
                String line =reader.readLine();
                if (line == null) {
                    break;
                }
                strb.append(line);
                strb.append("\n");
            }
        } catch (IOException ex) {
            
        } finally {
            try {
                reader.close();
            } catch (Exception ignore) { }
        }
        return strb.toString();

    }
    public static List<File> getAllFilesUnderDir(File dir, final FileFilter fileFilter) {
        FileFilter acceptDirFileFilterWrapper  = new FileFilter(){
                public boolean accept(File f)   {
                    if (f.isDirectory()) return true;
                    return   fileFilter.accept(f);
                }
            };
        ArrayList<File> files = new ArrayList<File>();
        Stack<File> s = new Stack<File>();
        s.push(dir);
        File tmp = null;
        while (!s.isEmpty()) {
            tmp = s.pop();
            if (tmp.isDirectory() && tmp.canRead() && tmp.canExecute()) {
                File[] cs = tmp.listFiles(acceptDirFileFilterWrapper);
                for (File c : cs) {
                    s.push(c);
                }
            } else if (tmp.isFile() && tmp.canRead()) {
                files.add(tmp);
            }
        }
        return files;
    }
    public List<File> getErls(File dir){
        return getAllFilesUnderDir(dir, new FileFilter(){
                public boolean accept(File file){
                    if(file.getName().endsWith(".erl")){
                        return true;
                    }
                    return false;
                }
            } );
        
    }
    public int countMatch(String str,String pattern){
        int i =0;
        Pattern p = Pattern.compile(pattern);
        Matcher m =p.matcher(str);
        while (m.find()){
            i++;
        }
        return i;
    }
    
    public static void main(String[] args) {
        if (args.length==0){
            System.out.println("need a param as directory name");
            return ;
        }
        long start= System.currentTimeMillis();
        Main m  = new Main();
        int countMatch1=0;
        int countMatch2=0;
        List<File> erls = m.getErls(new File(args[0]));
        for (Iterator it = erls.iterator(); it.hasNext(); ) {
            File file = (File)it.next();
            String str=m.read(file);
            countMatch1+=(m.countMatch(str,"if"));
            countMatch2+=(m.countMatch(str,"case"));
        }

        System.out.println("count of 'if' is " + countMatch1);
        System.out.println("count of 'case' is " + countMatch2);
        long end= System.currentTimeMillis();
        System.out.println("time :"+ (end-start) + " ms");

    }
}
