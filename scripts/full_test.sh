stack clean
stack test
stack runhaskell scripts/VisualTest.hs

stack clean
STACK_YAML=stack-8.0.yaml stack test
STACK_YAML=stack-8.0.yaml stack runhaskell scripts/VisualTest.hs
