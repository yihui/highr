library(testit)

test_pkg('highr', if (getRversion() >= '3.0.0') 'testit' else 'test-R2')
