package main.java.edu.stonybrook.cs.thread;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.PriorityQueue;
import java.util.Set;

public class InverseSemanticScoreThread extends Thread{
	private String from = null;
	private String[] toList = null;
	private String threadKey = null;
	private String parameterKey = null;
	private HashMap<String, Integer> toListMap = new HashMap<String, Integer>();
	HashMap<String, Double> nodeScoreMap = new HashMap<String, Double>();
	HashMap<Integer, EdgeNode> nodeMap = new HashMap<Integer, EdgeNode>();
	PriorityQueue<EdgeNode> queue = 
			new PriorityQueue<EdgeNode>(100, (a,b) -> (int)Math.ceil(b.curScore - a.curScore));
	private double cutOffVal = 0.005;
	private int depthLimit = 7;
	private ArrayList<String> prohibitedEdgeList = null;
	private int id = 0;
	
	public InverseSemanticScoreThread(String parameterKey, String threadKey, String from, String[] toList,
			ArrayList<String> prohibitedEdgeList)
	{
		this.parameterKey = parameterKey;
		this.from = from;
		this.toList = toList;
		this.threadKey = threadKey;
		int index = 0;
		for(String sid : this.toList)
		{
			toListMap.put(sid, index);
			index++;
		}
		this.prohibitedEdgeList = prohibitedEdgeList;
	}
	
	@Override
	public void run()
	{		
		EdgeNode node = new EdgeNode();
		node.edgeNodeSynsetID = from;
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
				if (targetNode.edgeNodeSynsetID.equals(from) || targetNode.edgeNodeSynsetID.equals("bn:02248101n") ||
						IsProhibitedEdge(targetNode.edgeType)) 
				{
					continue;
				} 
				else if(toListMap.containsKey(targetNode.edgeNodeSynsetID)) 
				{
					targetNode.SemConnectionNum = 
							BabelNetShareResource.GetSemanticConnectionNum(targetNode.edgeNodeSynsetID);
					targetNode.totalWeightedCount = targetNode.SemConnectionNum
							* targetNode.edgeWeight + nodeMap.get(parentNode.nodeID).totalWeightedCount;
					ArrayList<String> path = BabelNetShareResource.getPath(nodeMap, targetNode);
					targetNode.curScore = BabelNetShareResource.computeSemanticScore(parameterKey, targetNode.totalWeightedCount, path);
					BabelNetShareResource.setGlobalVal(threadKey, targetNode.curScore);
					String tmpAncestorPath = BabelNetShareResource.printPath(nodeMap, targetNode);
					if(targetNode.curScore > cutOffVal)
					{
						BabelNetShareResource.setScore(threadKey, toListMap.get(targetNode.edgeNodeSynsetID), targetNode.curScore,
								tmpAncestorPath);
					}
				} 
				else 
				{
					targetNode.SemConnectionNum = 
							BabelNetShareResource.GetSemanticConnectionNum(targetNode.edgeNodeSynsetID);
					targetNode.totalWeightedCount = targetNode.SemConnectionNum
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
