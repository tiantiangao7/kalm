package main.java.edu.stonybrook.cs.thread;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.PriorityQueue;
import java.util.Set;

public class SemanticScoreThread extends Thread
{
	private String from = null;
	private String[] toList = null;
	private HashMap<String, Integer> toListMap = new HashMap<String, Integer>();
	HashMap<String, Double> nodeScoreMap = new HashMap<String, Double>();
	HashMap<Integer, EdgeNode> nodeMap = new HashMap<Integer, EdgeNode>();
	PriorityQueue<EdgeNode> queue = 
			new PriorityQueue<EdgeNode>(100, (a,b) -> (int)Math.ceil(b.curScore - a.curScore));
	private String threadKey = null;
	private String ancestorPath = "";
	private double val = 0.0;
	private int index = -1;
	private double cutOffVal = 0.005;
	private int depthLimit = 7;
	private ArrayList<String> prohibitedEdgeList = null;
	private String parameterKey = null;
	private int id = 0;
	
	public SemanticScoreThread(String threadKey, int index, String s1, String[] s2,
			ArrayList<String> prohibitedEdgeList)
	{
		from = s1;
		toList = s2;
		this.index = index;
		this.threadKey = threadKey;
		this.prohibitedEdgeList = prohibitedEdgeList;
		for(String sid : this.toList)
		{
			toListMap.put(sid, 1);
		}
		parameterKey = Arrays.toString(toList);
	}
	
	@Override
	public void run() 
	{
		EdgeNode node = new EdgeNode();
		node.edgeNodeSynsetID = from;
		node.SemConnectionNum = BabelNetShareResource.GetSemanticConnectionNum(from);
		node.nodeID = id;
		id++;
		nodeMap.put(node.nodeID, node);
		queue.add(node);
		
		while (!queue.isEmpty()) {
			EdgeNode parentNode = queue.poll();
			double globalVal = BabelNetShareResource.getGlobalVal(threadKey); 
			
			if(parentNode.curScore < globalVal && parentNode.curScore > 0.00000001 && BabelNetShareResource.getPathLen(nodeMap, parentNode) > 2)
			{
				continue;
			}
			
			ArrayList<EdgeNode> parents = BabelNetShareResource.GetEdgeNodes(parameterKey, parentNode.edgeNodeSynsetID);
			for (EdgeNode targetNode : parents) {
				assert targetNode.edgeNodeSynsetID != null;
				assert targetNode.edgeWeight > 0.0;
				assert targetNode.edgeType != null;
				assert targetNode.parent == -1;
				assert targetNode.nodeID == -1;
				
				targetNode.parent = parentNode.nodeID;
				targetNode.nodeID = id;
				id++;
				
				globalVal = BabelNetShareResource.getGlobalVal(threadKey);
				if (targetNode.edgeNodeSynsetID.equals(from) || targetNode.edgeNodeSynsetID.equals("bn:02248101n")
						|| IsProhibitedEdge(targetNode.edgeType)) 
				{
					continue;
				} 
				else if (toListMap.containsKey(targetNode.edgeNodeSynsetID)) 
				{
					targetNode.totalWeightedCount = nodeMap.get(parentNode.nodeID).SemConnectionNum
							* targetNode.edgeWeight + nodeMap.get(parentNode.nodeID).totalWeightedCount;
					ArrayList<String> path = BabelNetShareResource.getPath(nodeMap, targetNode);
					targetNode.curScore = BabelNetShareResource.computeSemanticScore(parameterKey, targetNode.totalWeightedCount, path);
					String tmpAncestorPath = BabelNetShareResource.printPath(nodeMap, targetNode);
					if (targetNode.curScore > val)
					{
						val = targetNode.curScore;
						BabelNetShareResource.setGlobalVal(threadKey, val);
						ancestorPath = tmpAncestorPath;
					}
				} 
				else 
				{
					targetNode.SemConnectionNum = 
							BabelNetShareResource.GetSemanticConnectionNum(targetNode.edgeNodeSynsetID);
					targetNode.totalWeightedCount = nodeMap.get(parentNode.nodeID).SemConnectionNum
							* targetNode.edgeWeight + nodeMap.get(parentNode.nodeID).totalWeightedCount;
					ArrayList<String> path = BabelNetShareResource.getPath(nodeMap, targetNode);
					targetNode.curScore = BabelNetShareResource.computeSemanticScore(parameterKey, targetNode.totalWeightedCount, path);
					if ((targetNode.curScore >= globalVal && targetNode.curScore > cutOffVal &&
							path.size() < depthLimit) || (BabelNetShareResource.getPathLen(nodeMap, targetNode) <= 2))
					{						
						if(nodeScoreMap.containsKey(targetNode.edgeNodeSynsetID))
						{
							Double tempScore = nodeScoreMap.get(targetNode.edgeNodeSynsetID);
							if(tempScore < targetNode.curScore)
							{
								queue.add(targetNode);
								nodeScoreMap.put(targetNode.edgeNodeSynsetID, targetNode.curScore);
								nodeMap.put(targetNode.nodeID, targetNode);
							}
						}
						else
						{
							queue.add(targetNode);
							nodeScoreMap.put(targetNode.edgeNodeSynsetID, targetNode.curScore);
							nodeMap.put(targetNode.nodeID, targetNode);
						}
					}
				}
			}
		}			
		System.out.println("max: " + val);	
		if(val > cutOffVal)
		{
			BabelNetShareResource.setScore(threadKey, index, val, ancestorPath);
		}
		else
		{
			BabelNetShareResource.setScore(threadKey, index, 0.0, "");
		}
	}
	
	private boolean IsProhibitedEdge(String edge)
	{
		if(prohibitedEdgeList == null)
		{
			return false;
		}
		for(String tmp : prohibitedEdgeList)
		{
			if(edge.equals(tmp))
			{
				return true;
			}
		}
		return false;
	}
}
