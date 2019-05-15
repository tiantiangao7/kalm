package main.java.edu.stonybrook.cs.thread;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;

import main.java.edu.stonybrook.cs.frame.Frame;

public class BabelNetCache {
	private static HashMap<String, Double> BNSemanticConnectionScoreMap = new HashMap<String, Double>();
	private static HashMap<String, ArrayList<EdgeNode>> BNEdgeNodeMap = new HashMap<String, ArrayList<EdgeNode>>();
	
	public static void clear()
	{
		BNSemanticConnectionScoreMap.clear();
		BNEdgeNodeMap.clear();
	}
	
	public synchronized static void RecordBabelNetCall(String bncall)
	{
		try (BufferedWriter bw = new BufferedWriter(new FileWriter("resources/logs/log.txt", true))) 
		{
			bw.write(bncall + "\n");
		}
		catch (IOException x) 
		{
		      System.err.println(x);
			x.printStackTrace();
		}
	}
	
	public static Double GetCachedSemanticConnectionScore(String sid)
	{
		if(BNSemanticConnectionScoreMap.containsKey(sid))
		{
			return BNSemanticConnectionScoreMap.get(sid);
		}
		else
		{
			return null;
		}
	}
	
	public synchronized static void SetCachedSemanticConnectionScore(String sid, Double score)
	{
		if(!BNSemanticConnectionScoreMap.containsKey(sid))
		{
			BNSemanticConnectionScoreMap.put(sid, score);
			return;
		}
	}
	
	public static ArrayList<EdgeNode> GetCachedEdgeNodes(String sid)
	{
		if(BNEdgeNodeMap.containsKey(sid))
		{
			return BNEdgeNodeMap.get(sid);
		}
		else
		{
			return null;
		}
	}
	
	public synchronized static void SetCachedEdgeNodes(String sid, ArrayList<EdgeNode> nodeList)
	{
		if(!BNEdgeNodeMap.containsKey(sid))
		{
			BNEdgeNodeMap.put(sid, nodeList);
			return;
		}
	}
}
