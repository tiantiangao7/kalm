package main.java.edu.stonybrook.cs.fpparser;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import main.java.edu.stonybrook.cs.thread.EdgeNode;

public class SemanticLinkAddition {
	private static HashMap<String,ArrayList<EdgeNode>> semanticLinkAdditionMap = 
			new HashMap<String,ArrayList<EdgeNode>>();
	private static final String input = "resources/semantic_score_meta/semantic_link_augmentation.txt";
	
	public static void initialize()
	{
		Parse();
	}
	
	public static boolean HasAddedSemanticLinks(String synset)
	{
		return semanticLinkAdditionMap.containsKey(synset);
	}
	
	public static ArrayList<EdgeNode> GetAddedSemanticLinks(String synset)
	{
		return semanticLinkAdditionMap.get(synset);
	}
	
	private static void ParseSemanticLinkAugmentationPredicate(String input)
	{
	     Matcher m = Pattern.compile("^semantic_link_augmentation\\(\\'(.+)\\',\\'(.+)\\',\\'(.+)\\',(.+)\\)\\.$").matcher(input);
	     while(m.find()) {
	    	 EdgeNode node = new EdgeNode();
			 node.edgeNodeSynsetID = m.group(3);
			 node.edgeWeight = Double.parseDouble(m.group(4));
			 node.edgeType = m.group(2);
			 System.out.println(node.parent + " " + node.edgeType + " " + node.edgeNodeSynsetID + " " + node.edgeWeight);
			 if(semanticLinkAdditionMap.containsKey(m.group(1)))
			 {
				 semanticLinkAdditionMap.get(m.group(1)).add(node);
			 }
			 else
			 {
				ArrayList<EdgeNode> list = new ArrayList<EdgeNode>();
				list.add(node);
				semanticLinkAdditionMap.put(m.group(1), list);
			 }      
	     }	       	       
	}
	
	public static void Parse()
	{
		try (BufferedReader br = new BufferedReader(new FileReader(input))) 
		{			
			String sentence;			
			while((sentence = br.readLine())!=null)
			{
				ParseSemanticLinkAugmentationPredicate(sentence);       
			}		
		}
		catch (IOException x) 
		{
		      System.err.println(x);
			x.printStackTrace();
		}
	}
}
