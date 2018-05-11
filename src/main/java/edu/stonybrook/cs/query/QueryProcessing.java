package main.java.edu.stonybrook.cs.query;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

public class QueryProcessing {
	private static Set<String> varWordIndexSet = new HashSet<String>();

	public static void PreProcessQuery(String input) {
		varWordIndexSet.clear();
		varWordIndexSet.add("w0.0");
		ExtractExplicitVar(input);
	}

	private static void ExtractExplicitVar(String input) {
		int sentenceID = 1;
		String[] temp = input.substring(0, input.length() - 1).split("(?=')|(?<=')|(?=:)|(?<=:)|(?=\\s+)|(?<=\\s+)");
		int index = 1;
		for (String word : temp) {
			if (word.trim().length() > 0) {
				System.out.println(word + " - w" + sentenceID + "." + index);
				if (word.startsWith("$")) {
					varWordIndexSet.add("w" + sentenceID + "." + index);
					System.out.println("explicit var: w" + sentenceID + "." + index);
				}
				index++;
			}
		}
	}

	public static void ExtractImplicitVar()
	{
		try (BufferedReader br = 
				new BufferedReader(new FileReader("scripts/prolog/ape/tmp/implicit_var.txt"))) 
		{
			String line = br.readLine();
			while(line != null)
			{
				varWordIndexSet.add(line);
				System.out.println("implicit var: " + line);
				line = br.readLine();
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public static boolean IsQuery(String input) {
		if (input.endsWith("?")) {
			return true;
		} else {
			return false;
		}
	}

	public static boolean IsVarWordIndexSetEmpty() {
		return varWordIndexSet.isEmpty();
	}

	public static boolean IsInVarWordIndexSet(String index)
	{
		return varWordIndexSet.contains(index);
	}
	
	public static void ClearVarWordIndexSet()
	{
		varWordIndexSet.clear();
	}
}
