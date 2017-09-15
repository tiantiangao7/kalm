package main.java.edu.stonybrook.cs.thread;

public class EdgeNode {
	public String edgeNodeSynsetID = null;
	public Double edgeWeight = 0.0;
	public String edgeType = null;
	public Double totalWeightedCount = 0.0;
	public Double SemConnectionNum = 0.0;
	public String parent = null;
	public Double curScore = 0.0;
	
	public EdgeNode(EdgeNode e)
	{
		this.edgeNodeSynsetID = e.edgeNodeSynsetID;
		this.edgeWeight = e.edgeWeight;
		this.edgeType = e.edgeType;
		this.totalWeightedCount = e.totalWeightedCount;
		this.SemConnectionNum = e.SemConnectionNum;
		this.parent = e.parent;
		this.curScore = e.curScore;
	}
	
	public EdgeNode()
	{
		
	}
}
