package main.java.edu.stonybrook.cs.thread;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import it.uniroma1.lcl.babelnet.BabelNet;
import it.uniroma1.lcl.babelnet.BabelSynset;
import it.uniroma1.lcl.babelnet.BabelSynsetID;
import it.uniroma1.lcl.babelnet.BabelSynsetIDRelation;
import it.uniroma1.lcl.babelnet.InvalidBabelSynsetIDException;
import it.uniroma1.lcl.babelnet.data.BabelPointer;
import main.java.edu.stonybrook.cs.fpparser.SemanticLinkOverride;
import main.java.edu.stonybrook.cs.fpparser.SemanticScoreParameters;

public class BabelNetShareResource {
	private static String rootSID = "bn:00031027n";
	private static HashMap<String,Double> scoreMap = new HashMap<String,Double>();
	private static HashMap<String,String> pathMap = new HashMap<String,String>();
	private static HashMap<String, Double> globalValMap = new HashMap<String, Double>();
	
	public static Double getGlobalVal(String threadKey)
	{
		if(globalValMap.containsKey(threadKey))
		{
			return globalValMap.get(threadKey);
		}
		else
		{
			return 0.0;
		}
	}
	
	public synchronized static void setGlobalVal(String threadKey, Double val)
	{
		if(!globalValMap.containsKey(threadKey))
		{
			globalValMap.put(threadKey, val);
			return;
		}
		else
		{
			if(val > globalValMap.get(threadKey))
			{
				globalValMap.put(threadKey, val);
			}
			return;
		}
	}
	
	public static void clear()
	{
		scoreMap.clear();
		pathMap.clear();
		globalValMap.clear();;
	}
	
	public static String getPath(String threadKey, int index)
	{
		return pathMap.get(threadKey + ":" + index);
	}
	
	public static Double getScore(String threadKey, int index)
	{
		return scoreMap.get(threadKey + ":" + index);
	}
	
	public synchronized static void setScore(String threadKey, int index, Double val, String path)
	{
		Double score = -1.0;
		if(scoreMap.containsKey(threadKey + ":" + index))
		{
			score = scoreMap.get(threadKey + ":" + index);
		}
		
		if(score < val)
		{
			scoreMap.put(threadKey + ":" + index, val);
			pathMap.put(threadKey + ":" + index, path);
		}
	}
	
	public static Double GetSemanticConnectionNum(String sid)
	{
		Double cachedScore = BabelNetCache.GetCachedSemanticConnectionScore(sid);
		
		if(cachedScore != null)
		{
			return cachedScore;
		}
		
		BabelNet bn = BabelNet.getInstance();
		BabelSynset by = null;
		try {
			by = bn.getSynset(new BabelSynsetID(sid));
		} catch (IOException e) {
			e.printStackTrace();
		} catch (InvalidBabelSynsetIDException e) {
			e.printStackTrace();
		}
		Double score = Math.sqrt(by.getEdges().size()*1.0);
		BabelNetCache.SetCachedSemanticConnectionScore(sid, score);
		return score;
	}
	
	public static ArrayList<EdgeNode> GetEdgeNodes(String parameterKey, String s1)
	{
		ArrayList<EdgeNode> cachedEdgeNodeList = BabelNetCache.GetCachedEdgeNodes(s1);
		if(cachedEdgeNodeList != null)
		{
			ArrayList<EdgeNode> result = new ArrayList<EdgeNode>();
			for(EdgeNode node : cachedEdgeNodeList)
			{
				EdgeNode tmpNode = new EdgeNode(node);
				result.add(tmpNode);
			}
			return result;
		}
		
		BabelNet bn = BabelNet.getInstance();
		HashMap<String,EdgeNode> nodeMap = new HashMap<String,EdgeNode>();
		ArrayList<EdgeNode> edgeNodeList = new ArrayList<EdgeNode>();
			
		BabelSynset by;
		try {
			by = bn.getSynset(new BabelSynsetID(s1));
			for(BabelSynsetIDRelation edge : by.getEdges()) {
				Double weight = getAdjustedEdgeWeight(parameterKey, edge);
				if(weight > 0)
				{
					String targetSID = edge.getBabelSynsetIDTarget().getID();
					if(SemanticLinkOverride.isSemanticLinkOverridden(s1+"-"+getEdgeType(edge.getPointer())+"-"+targetSID))
					{
						continue;
					}
					
					if(nodeMap.containsKey(targetSID))
					{
						EdgeNode node = nodeMap.get(targetSID);
						if(weight > node.edgeWeight)
						{
							node.edgeWeight = weight;
							node.edgeType = getEdgeType(edge.getPointer());
						}
					}
					else
					{	EdgeNode node = new EdgeNode();
						node.edgeNodeSynsetID = targetSID;
						node.edgeWeight = weight;
						node.edgeType = getEdgeType(edge.getPointer());
						node.parent = s1;
						nodeMap.put(targetSID, node);
					}  
				}    
	        }
		} catch (IOException e) {
			e.printStackTrace();
		} catch (InvalidBabelSynsetIDException e) {
			e.printStackTrace();
		}		
		List<String> keyList = new ArrayList<String>(nodeMap.keySet());
		for(String key : keyList)
		{
			edgeNodeList.add(nodeMap.get(key));
		}
		BabelNetCache.SetCachedEdgeNodes(s1, edgeNodeList);
		ArrayList<EdgeNode> result = new ArrayList<EdgeNode>();
		for(EdgeNode node : edgeNodeList)
		{
			EdgeNode tmpNode = new EdgeNode(node);
			result.add(tmpNode);
		}
		return result;
	}
	
	private static String getEdgeType(BabelPointer bp)
	{
		if(bp.isHypernym())
		{
			return "hypernym";
		}
		else if(bp.isHyponym())
		{
			return "hyponym";
		}
		else if(bp.isHolonym())
		{
			return "holonym";
		}
		else if(bp.isMeronym())
		{
			return "meronym";
		}
		else if(bp.getName().equals("Derivationally related form"))
		{
			return "Derivationally related form";
		}
		else if(bp.getName().equals("Gloss related form (disambiguated)"))
		{
			return "Gloss related form (disambiguated)";
		}
		else if(bp.getName().equals("Gloss related form (monosemous)"))
		{
			return "Gloss related form (monosemous)";
		}
		else
		{
			return "other";
		}
	}
	
	private static Double getAdjustedEdgeWeight(String parameterKey, BabelSynsetIDRelation edge)
	{
		Double weight = edge.getWeight();
		String edgeType = getEdgeType(edge.getPointer());
		return weight + SemanticScoreParameters.getWeightBias(parameterKey + "::" + edgeType);
	}
	
	public static double computeSemanticScore(String parameterKey, Double weightedCountTotal, ArrayList<String> path)
	{
		double numerator = weightedCountTotal * 1.0;
		double denominator = 0.0;
		
		for(String edge : path)
		{
			denominator += SemanticScoreParameters.getPenaltyVal(parameterKey + "::" + edge);
		}
		return numerator/Math.pow(5.0, denominator);
	}
	
	public static ArrayList<String> getPath(HashMap<String, EdgeNode> nodeMap, EdgeNode node)
	{
		ArrayList<String> path = new ArrayList<String>();
		path.add(node.edgeType);
		String parent = node.parent;
		
		while(true)
		{
			EdgeNode tempNode = nodeMap.get(parent);
			if(tempNode.parent == null)
			{
				break;
			}
			else
			{
				path.add(tempNode.edgeType);
				parent = tempNode.parent;
			}
		}
		return path;
	}
	
	public static String printPath(HashMap<String, EdgeNode> nodeMap, EdgeNode node)
	{
		String pathResult = "";
		ArrayList<String> path = new ArrayList<String>();
		path.add(node.edgeType);
		String parent = node.parent;
		System.out.print(node.edgeNodeSynsetID + ":" + node.totalWeightedCount + " ");
		pathResult += node.edgeNodeSynsetID + ":" + node.totalWeightedCount;
		pathResult += "-" + node.edgeType + "-";
		while(true)
		{
			EdgeNode tempNode = nodeMap.get(parent);
			System.out.print(parent + ":" + tempNode.totalWeightedCount + " ");
			pathResult += parent + ":" + tempNode.totalWeightedCount;
			if(tempNode.parent == null)
			{
				break;
			}
			else
			{
				path.add(tempNode.edgeType);
				parent = tempNode.parent;
				pathResult += "-" + tempNode.edgeType + "-";
			}
		}
		
		System.out.print(node.curScore + "\n");
		for(String edge : path)
		{
			System.out.print(edge + " ");
		}
		System.out.print("\n");
		return pathResult;
	}
	
	public static String getInverseEdgeName(String edgeName)
	{
		if(edgeName.equals("hypernym"))
		{
			return "hyponym";
		}
		else if(edgeName.equals("hyponym"))
		{
			return "hypernym";
		}
		else if(edgeName.equals("holonym"))
		{
			return "meronym";
		}
		else if(edgeName.equals("meronym"))
		{
			return "holonym";
		}
		else {
			return edgeName;
		}
	}
}
