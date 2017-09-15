package main.java.edu.stonybrook.cs.util;

public class PrologConnector {
	
	private static String XSBBin = "/Users/tiantiangao/Documents/Research/KnowledgeAqcuisition/Software/Knowledge Acquisition Machine/kam/runxsb.sh";

	public static void ExecutePrologQuery() {
		Process p;
		try {
			String[] cmd = { XSBBin };

			p = Runtime.getRuntime().exec(cmd);
			p.waitFor();	
			p.destroy();
		} catch (Exception e) {
			System.out.println("Prolog Exception: ");
			e.printStackTrace();
		}
	}
	
	public static void main(String[] args){
		ExecutePrologQuery();
		System.out.println("End.");
	}
}
