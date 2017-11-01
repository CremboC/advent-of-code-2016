pipeline {
  agent any
  stages {
    stage('Install GHC') {
      steps {
        tool 'ghc'
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