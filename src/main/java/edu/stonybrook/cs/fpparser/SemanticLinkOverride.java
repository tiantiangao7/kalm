package main.java.edu.stonybrook.cs.fpparser;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import main.java.edu.stonybrook.cs.batch.Batch;

public class SemanticLinkOverride {
	private static Set<String> overrideSet = new HashSet<String>();
	private static final String input = "resources/semantic_score_meta/semantic_link_override.txt";
	
	public static void initialize()
	{
		Parse();
	}
	
	public static boolean isSemanticLinkOverridden(String link)
	{
		if(overrideSet.contains(link))
		{
			return true;
		}
		else
		{
			return false;
		}
	}
	
	private static void ParseSemanticLinkOverridePredicate(String input)
	{
	     Matcher m = Pattern.compile("^semantic_link_override\\(\\'(.+)\\',\\'(.+)\\',\\'(.+)\\'\\).$").matcher(input);
	     while(m.find()) {	    	 
	    	 overrideSet.add(m.group(1) + "-" + m.group(2) + "-" + m.group(3));      
	     }	       	       
	}
	
	public static void Parse()
	{
		try (BufferedReader br = new BufferedReader(new FileReader(input))) 
		{			
			String sentence;			
			while((sentence = br.readLine())!=null)
			{
				ParseSemanticLinkOverridePredicate(sentence);       
			}		
		}
		catch (IOException x) 
		{
		      System.err.println(x);
		}
	}
}
