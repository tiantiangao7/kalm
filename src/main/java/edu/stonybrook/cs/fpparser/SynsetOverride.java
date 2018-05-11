package main.java.edu.stonybrook.cs.fpparser;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class SynsetOverride {
	private static Set<String> synsetOverrideSet = new HashSet<String>();
	private static final String input = "resources/semantic_score_meta/synset_override.txt";
	
	public static void initialize()
	{
		Parse();
	}
	
	public static boolean isSynsetOverridden(String synset)
	{
		if(synsetOverrideSet.contains(synset))
		{
			return true;
		}
		else
		{
			return false;
		}
	}
	
	private static void ParseSynsetOverridePredicate(String input)
	{
	     Matcher m = Pattern.compile("^synset_override\\(\\'(.+)\\',\\'(.+)\\'\\)\\.$").matcher(input);
	     while(m.find()) {
	    	 System.out.println(m.group(1) + "-" + m.group(2));
	    	 synsetOverrideSet.add(m.group(1) + "-" + m.group(2));      
	     }	       	       
	}
	
	public static void Parse()
	{
		try (BufferedReader br = new BufferedReader(new FileReader(input))) 
		{			
			String sentence;			
			while((sentence = br.readLine())!=null)
			{
				ParseSynsetOverridePredicate(sentence);       
			}		
		}
		catch (IOException x) 
		{
		      System.err.println(x);
		}
	}
}
