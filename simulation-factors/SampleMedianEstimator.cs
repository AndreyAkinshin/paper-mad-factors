using Perfolizer.Mathematics.QuantileEstimators;

namespace Simulation;

public class SampleMedianEstimator : HyndmanFanQuantileEstimator
{
    public static readonly IQuantileEstimator Instance = new SampleMedianEstimator();

    private SampleMedianEstimator() : base(HyndmanFanType.Type7)
    {
    }

    public override string Alias => "SM";
}