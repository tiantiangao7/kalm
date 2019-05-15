package main.java.edu.stonybrook.cs.util;

public class PrologConnector_tmp {
	
	// ToDo: we have to move this to a property file
	private static String XSBBin = "C:\\Users\\Paul\\Documents\\_git2\\knowledge-acquisition\\Software\\Knowledge Acquisition Machine\\kam\\runxsb.bat";

	public static void ExecutePrologQuery() {
		Process p;
		try {
			String[] cmd = { XSBBin };

			System.out.println("Calling XSB");
			p = Runtime.getRuntime().exec(cmd);
			System.out.println("Executing XSB");

			System.out.print("cmd = [ ");
			for(String s:cmd)
				System.out.println("  " + s);
			System.out.println("]");
			
			p.waitFor();
			p.destroy();

			System.out.println("Finished XSB");

		} catch (Exception e) {
			System.out.println("XSB exception");
			System.out.println("Prolog Exception: ");
			e.printStackTrace();
		}
	}
	
	public static void main(String[] args){
		ExecutePrologQuery();
		System.out.println("End.");
	}
}
