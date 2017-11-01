pipeline {
  agent any
  stages {
    stage('Install GHC') {
      steps {
        sh '''#!/bin/sh
apt-get update && apt-get install ghc6'''
      }
    }
    stage('Test Day 1') {
      steps {
        sh '''#!/bin/sh
cd day-01 && ghc -o runnable day1.hs && ./runnable'''
      }
    }
  }
}