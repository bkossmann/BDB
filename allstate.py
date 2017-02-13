# -*- coding: utf-8 -*-
"""
Created on Sat Dec  3 13:43:26 2016

NOTE: TO USE THIS SCRIPT, ALL-CAPS HEADERS SHOULD BE UNBLOCKED INDIVIDUALLY.
I KNOW THAT'S A HACK! GIVEN MORE INCENTIVE TO CLEAN THIS UP, A MUCH BETTER
SOLUTION WOULD BE FORTHCOMING.

I would normally split the functions into separate libraries. Probably not
worth the time in this case.
@author: Brad
"""
import numpy as np
import random
#import math
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
#from sklearn import naive_bayes, linear_model
#import time
from scipy import stats
#from collections import Counter
#import operator
from sklearn import cluster, decomposition

'''Bring in the training set (and test set if test set to true)'''
def traintest_in(filename, test=False):
    data = []
    tid = []
    filein = open(filename,'r')
    filein.next()
    for line in filein:
        lines = line.strip('\n').split(',')
        data.append([x for x in lines[1:]])
        tid.append(lines[0])
        for i in range(116,len(data[len(data) - 1]) ):
            data[len(data) - 1][i] = float(data[len(data) - 1][i])
            
    filein.close()
    return data, tid
    
'''Outputs Kaggle-format predictions'''
def submit_out(id_loss):
    fileout = open('submit.csv','w')
    fileout.write('id,loss\n')
    for i in range(len(id_loss)):
        fileout.write(str(id_loss[i][0])+','+str(id_loss[i][1])+'\n')
    fileout.close()

'''Bring in the calculated modifiers'''
def mod_mult_in():
    mod_dict = {}
    modfile = open('mod_mult.dat','r')
    for line in modfile:
        lines = line.split()
        if int(lines[0]) in mod_dict:
            mod_dict[int(lines[0])][lines[1]] = float(lines[2])
        else:
            mod_dict[int(lines[0])] = { lines[1] : float(lines[2]) }
            
    modfile.close()
    return mod_dict
    
'''Useful to remove outliers from training set'''
def subset(data, col, cutoff, highlow):
    poplist=[]
    if highlow=='high':
        for i in range(len(data)):
            if data[i][col] < cutoff:
                poplist.append(i)
                
    elif highlow=='low':
        for i in range(len(data)):
            if data[i][col] > cutoff:
                poplist.append(i)
                
    poplist.reverse()
    for x in poplist:
        data.pop(x)
        
    return(data)
        
'''Split the training set into separare features and claims severity lists'''
def split_train(data):
    data = zip(*data)
    losses = data[len(data) - 1]
    features = zip(*data[:len(data) - 1])
    return losses, features

'''This function not used'''
def norm_corr(data, col1, col2): #Normalize two columns, then calc x-corr
#Note - normalized cross-correlation not yet implemented in numpy. Hence the
#extra code.
    temp = zip(*data)
    set1 = list(temp[col1])
    set2 = list(temp[col2])
    std1 = np.std(set1)
    avg1 = np.mean(set1)
    std2 = np.std(set2)
    avg2 = np.mean(set2)
    for i in range(len(set1)):
        set1[i] = (set1[i] - avg1) / std1
        set2[i] = (set2[i] - avg2) / std2
        
    cov = np.dot(set1, set2)/len(set1)
    
    return set1, set2, cov
'''This function not used'''   
def qual_to_quant(data, col):
    temp = zip(*data)
    temp = list(temp[col])
    unique = list(set(temp))
    trans = {}
    for i in range(len(unique)):
        trans[unique[i]] = i
    for i in range(len(temp)):
        temp[i] = trans[temp[i]]
        
    return temp, dict(zip(trans.values(), trans.keys()))
 
'''Create randomized (smaller) training set to speed up model fitting'''   
def randomize_subset(data, percentage):
    p = int((float(len(data)) * percentage) / 100)
    subset = random.sample(data, p)
    
    return subset
    
'''Uses the simple linear model based only on continuous features'''
def simple_model(features, mb, weights, fudge):
    estimates = []
    for i in range(len(features)):
        est = 0
        for j in range(len(features[i])):
            est = est + fudge + (mb[j][0] * features[i][j] + mb[j][1]) * weights[j]
        estimates.append(est)
        
    return estimates
    
'''Applies categorical feature value modifiers'''
def apply_mods(features, mult_mods, estimates, scale):
    new_estimates=[]
    mod_det = []
    for i in range(len(features)):
        new_estimates.append(estimates[i])
        mod_det.append([])
        for j in range(1, 117):
            try:
                new_estimates[i] = new_estimates[i] + mult_mods[j][features[i][j]]/scale
                mod_det[i].append(mult_mods[j][features[i][j]])
            except: #If the category doesn't have a modifier
                pass
            
    new_estimates = [np.exp(x) for x in new_estimates]
    return new_estimates, mod_det
    
'''Find differences between predictions and actual claims severities in 
training set'''
def get_errors(estimates, observations):
    diff=[]
    for i in range(len(estimates)):
        diff.append(estimates[i] - observations[i])
        
    me = np.mean(diff)
    me_std = np.std(diff)
    
    diff2 = [abs(x) for x in diff]
    
    mae = np.mean(diff2)
    mae_std = np.std(diff2)
    
    return mae, mae_std, me, me_std, diff
    
'''Find error estimates for each severity bin'''
def slice_error_calibrate(mods, estimates, errors, width=1000):
    est_errors = []
    sort = np.argsort(estimates)
    est = np.array(estimates)[sort]
    err = np.array(errors)[sort]
    est_bounds = [est[0]]
    for i in range(width, len(est), width):
        try:
            est_bounds.append(est[i - 1])
        except:
            est_bounds.append(est[len(est) - 1])
        try:    
            est_errors.append(np.median(err[i - width:i]))
        except:
            est_errors.append(np.median(err[i - width:len(err)]))
            
    mod_sums = [sum(i) for i in mods]
    mod_errors = []
    sort = np.argsort(mod_sums)
    mod = np.array(mod_sums)[sort]
    err = np.array(errors)[sort]
    mod_bounds = [mod[0]]
    for i in range(width, len(mod), width):
        try:
            mod_bounds.append(mod[i - 1])
        except:
            mod_bounds.append(mod[len(mod) - 1])
        try:    
            mod_errors.append(np.median(err[i - width:i]))
        except:
            mod_errors.append(np.median(err[i - width:len(err)]))    
    
    return est_bounds, est_errors, mod_bounds, mod_errors
    
'''Simple recursive binning function'''
def do_bin(x, boundaries, old_bounds): #Sloppy, probably can be improved
    mid = len(boundaries)/2

    if len(boundaries) < 2:
        if x <= old_bounds[1]:
            return 0
        elif x >= old_bounds[len(old_bounds) - 1]:
            return len(old_bounds) - 2
        else: 
            return old_bounds.index(boundaries[0]) - 1
    elif x > boundaries[mid]:
        return do_bin(x, boundaries[mid:], old_bounds)
    elif x < boundaries[mid]:
        return do_bin(x, boundaries[:mid], old_bounds)
    elif x == boundaries[mid]:
        return old_bounds.index(boundaries[mid]) - 1
        
'''Check to see if slice-error correction is effective'''
def test_slice_error(bounds, est_errors, estimates, errors):
    err_diff = []
    #Cannot use np.digitize because bins are not monotonic
    
    print 'binning may take awhile'
    err_bins = [do_bin(i, bounds, bounds) for i in estimates]
    
    print 'binning finished'
    for i in range(len(err_bins)):
        err_diff.append(errors[i] - est_errors[err_bins[i]])
        
    print 'MAE, adjusted, unadjusted', \
          np.mean([abs(x) for x in err_diff]), np.mean([abs(x) for x in diff])
          
    return err_diff
        
        
'''MAIN'''


train, junk = traintest_in('train.csv')

'''Training on a lower-severity subset of the data seems to work better than
training on the whole set. Throwing out the high-outliers makes for better
statistics.'''
train = subset(train, len(train[0]) - 1, 25000, "low")
train = randomize_subset(train, 5)
test, test_id = traintest_in('test.csv')

losses, features = split_train(train)

qual_features = zip(*zip(*train)[0:116])
cont_features = zip(*zip(*train)[116:len(train[0]) - 1])


'''CLUSTERING'''
'''
km = cluster.KMeans(n_clusters=5)

cluster_list = km.fit_predict(cont_features, losses)

clust_dict = {}

for i in range(len(cluster_list)):
    if cluster_list[i] not in clust_dict:
        clust_dict[cluster_list[i]] = [losses[i]]
    else:
        clust_dict[cluster_list[i]].append(losses[i])
        
clust_sort = []
for clus, losses in clust_dict.iteritems():
    clust_sort.append(losses)
    
for i in range(len(clust_sort)):
    plt.hist(clust_sort[i], alpha=0.5, normed=True, bins=100)
    
    plt.show()
'''

'''PCA and ICA'''
'''
PC = decomposition.PCA()
projections = PC.fit_transform(cont_features)
projections = zip(*projections)

for i in range(len(projections)):
    plt.scatter(projections[i], losses, s=2)
    plt.show()

#ICA
PC = decomposition.FastICA()
ICAprojections = PC.fit_transform(cont_features)
ICAprojections = zip(*ICAprojections)


for i in range(len(ICAprojections)):
    plt.scatter(ICAprojections[i], losses, s=2)
    plt.show()
'''

'''FIND CURVES'''
weights=[]
curves=[]
features_inv = zip(*features)

for i in range(116, len(train[0]) - 1):
    m, b, r, p, ste = stats.linregress(features_inv[i], losses)
    curves.append([m,b])
    weights.append(r)
    
for i in range(len(weights)):
    weights[i] = abs(weights[i])
    
s = sum(weights)
weights = [w/s for w in weights]

'''FIND MODIFIERS'''
losses = [np.log(x) for x in losses]

loss_mean = np.mean(losses)
mod_mult = []
for i in range(1,117):
    print i
    temp={}
    for j in range(0,len(train)):
        if train[j][i] in temp:
            temp[train[j][i]].append(losses[j])
        else:
            temp[train[j][i]] = [losses[j]]

    for mod, loss in temp.iteritems():
        if len(loss) >= 100: #3000
            mod_mean = np.median(loss) #Slightly more effective than mean
            mod_mult.append([str(i), str(mod), str(mod_mean - loss_mean)])

modout = open('mod_mult.dat', 'w')
for i in range(len(mod_mult)):
    modout.write(mod_mult[i][0]+' '+mod_mult[i][1]+' '+mod_mult[i][2]+'\n')
modout.close()

mods = mod_mult_in()   

est = simple_model(cont_features, curves, weights, 0)
est_nomod = [np.log(x) for x in est]

mae, mae_std, me, me_std, diff = get_errors([np.exp(x) for x in est_nomod], [np.exp(x) for x in losses])
print 'no mods', mae, mae_std, me, me_std
plt.hist(diff, bins=100, normed=True)
plt.show()

est = [np.log(x) for x in est]
est_mod, m_det = apply_mods(qual_features, mods, est, 5.4) #5.4 empirically found to work best

mae, mae_std, me, me_std, diff = get_errors(est_mod, [np.exp(x) for x in losses])
print 'with mods', mae, mae_std, me, me_std
plt.hist(diff, bins=100, normed=True, range=(-10000,10000))
plt.show()

'''For checking how much of distribution falls within a certain error'''
'''
for i in range(100,3000,100):
    count=0
    for x in diff:
        if abs(x) < i:
            count+=1
    print i, float(count)/float(len(diff))
'''
    
'''
plt.hist(diff, bins=100, normed=True)
plt.show()

plt.hist(est_mod, bins=100, alpha=0.2, normed=True, label='est')
plt.hist([np.exp(x) for x in losses], bins=100, alpha=0.2, normed=True, label='exp')
plt.legend(loc='upper right')
plt.show()
'''

for i in range(len(m_det)):
    m_det[i] = [np.exp(x) for x in m_det[i]]

losses = [np.exp(x) for x in losses]


est_bounds, est_errors, mod_bounds, mod_errors = \
slice_error_calibrate(m_det, est_mod, diff )

est_diff = test_slice_error(est_bounds, est_errors, est_mod, diff)
plt.hist(diff, bins=100, range=(-10000,10000), normed=True)
plt.show()


'''
plt.scatter(est_diff, losses, s=0.1)
plt.show()

plt.scatter(mod_diff, losses, s=0.1)
plt.show()

plt.scatter(diff, losses, s=0.1)
plt.show()
'''

'''FOR OUTPUT'''
'''
test_features = zip(*zip(*test)[116:len(train[0]) - 1])
test_qual = zip(*zip(*test)[0:116])
est = simple_model(test_features, curves, weights, 0)
est_mod = apply_mods(test_qual, mods, est, 5)
out = zip(*[test_id,est_mod])
submit_out(out)
'''
