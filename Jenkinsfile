pipeline {
  agent any
  stages {
    stage('Test Day 1') {
      steps {
        sh '''#!/bin/sh
cd day-01 && ghc -o runnable day1.hs && ./runnable'''
      }
    }
  }
}