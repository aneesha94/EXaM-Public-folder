from __future__ import division
from __future__ import absolute_import
import numpy as np

import xlrd

from scipy.stats import norm

folder = u"C:/Users/ap2474/Dropbox/ExperimentalDesignAsMarketDesign/aneesha"

def AAIW(Y, U, EU, EUU, Z, n):

    # Y: n x 1 np.matrix

    # U: n x m np.matrix (m is # of causes)

    # EU: n x m np.matrix

    # EUU: n x m x m np.array

    # Z: n x l np.matrix (l is # of attributes)

    # n: population size (= sample size)

    # i: the ongoing loop


    EUZ = EU.T*Z

    ZZ_inv = (Z.T*Z).I

    Lambda = EUZ*ZZ_inv

    X = U - Z*Lambda.T

    W = np.append(X, Z, axis=1)   # W = [X, Z]

    WW_inv = (W.T*W).I

    beta = WW_inv*(W.T*Y) # beta = [theta', gamma']'

    H = np.mat(np.sum(EUU, axis=0))/n - Lambda*EUZ.T/n

    res = Y - W*beta

    Xres = np.multiply(X, res)

    G = (Xres.T*Z)*ZZ_inv

    A = Xres - Z*G.T

    DeltaZ = A.T*A/n

    AVar = H.I*DeltaZ*H.I/n

    SE = np.sqrt(np.diag(AVar))

    pvalue = 2 * norm.cdf(-abs(beta[0]/SE))

    print ([beta[0], SE, pvalue])

    return ([beta[0], SE, pvalue])





# Example


Dat = np.zeros(shape=(1000,3))
Name = [folder+u"/temp/07_dataexam_AAIW_"+str(i+1)+u".xlsx" for i in range(1000)]

for i in range (1000):
    book = xlrd.open_workbook(Name[i])
    sheet = book.sheet_by_index(0)
    n = sheet.nrows
    Y = np.matrix(sheet.col_values(2)[0:n]).T
    U = np.matrix(sheet.col_values(1)[0:n]).T
    EU = np.matrix(sheet.col_values(0)[0:n]).T
    Z = np.matrix(sheet.col_values(0)[0:n]).T
    ones = np.mat(np.ones(n)).T
    Z = np.append(Z,ones,axis=1)
    EUU = EU
    Dat[i] = AAIW(Y,U,EU,EUU,Z,n)

np.savetxt(folder+u'/input/AAIWexam.csv', Dat, delimiter=u',')


Dat = np.zeros(shape=(1000,3))
Name = [folder + u"/temp/07_datarct_AAIW_"+str(i+1)+u".xlsx" for i in range(1000)]

for i in range (1000):
    book = xlrd.open_workbook(Name[i])
    sheet = book.sheet_by_index(0)
    n = sheet.nrows
    Y = np.matrix(sheet.col_values(2)[0:n]).T
    U = np.matrix(sheet.col_values(0)[0:n]).T
    EU = np.matrix(sheet.col_values(1)[0:n]).T
    Z = np.mat(np.ones(n)).T
    EUU = EU
    Dat[i] = AAIW(Y,U,EU,EUU,Z,n)

np.savetxt(folder+u'/input/AAIWrct.csv', Dat, delimiter=u',')
