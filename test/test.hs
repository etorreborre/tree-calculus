import AutoDiscoveredSpecs   (tests)
import Test.Tasty.Extensions

main = tests >>= defaultMain . groupByModuleName
