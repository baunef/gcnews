import numpy as np
import pandas as pd

from sklearn.metrics import classification_report, accuracy_score
from sklearn.model_selection import StratifiedKFold, train_test_split

from npc_gzip.compressors.base import BaseCompressor
from npc_gzip.compressors.gzip_compressor import GZipCompressor
from npc_gzip.knn_classifier import KnnClassifier

from data import load_custom_dataset
from time import sleep

DATA_DIR = r"..\..\data"


def get_data() -> tuple:
    """
    Pulls the custom dataset
    and returns two tuples the first being the
    training data and the second being the test
    data. Each tuple contains the text and label
    respectively as numpy arrays.

    """
    train_iter, test_iter = load_custom_dataset(DATA_DIR)

    train_text = []
    train_labels = []
    for text, label in train_iter:
        train_labels.append(int(label))
        train_text.append(text)

    test_text = []
    test_labels = []
    for text, label in test_iter:
        test_labels.append(int(label))
        test_text.append(text)

    train_text = np.array(train_text)
    train_labels = np.array(train_labels)

    test_text = np.array(test_text)
    test_labels = np.array(test_labels)

    train = (train_text, train_labels)
    test = (test_text, test_labels)

    return (train, test)


def fit_model(
    train_text: np.ndarray, train_labels: np.ndarray, distance_metric: str = "ncd"
) -> KnnClassifier:
    """
    Fits a Knn-GZip compressor on the train
    data and returns it.

    Arguments:
        train_text (np.ndarray): Training dataset as a numpy array.
        train_labels (np.ndarray): Training labels as a numpy array.

    Returns:
        KnnClassifier: Trained Knn-Compressor model ready to make predictions.
    """

    compressor: BaseCompressor = GZipCompressor()
    model: KnnClassifier = KnnClassifier(
        compressor=compressor,
        training_inputs=train_text,
        training_labels=train_labels,
        distance_metric=distance_metric,
    )

    return model


def infer_predict(
        model: KnnClassifier, unlabeled_data: pd.DataFrame, top_k: int
) -> None:
    """

    :param model: a fitted KnnClassifier model
    :param unlabeled_texts: a Pandas DataFrame with a "Content" column containing texts
    :param top_k: integer, k for classifier
    :return: None
    """
    print("\nBeginning Inference!")

    # Inference
    (distances, labels, similar_samples) = model.predict(
        x=unlabeled_data["Content"],
        top_k=top_k
    )

    unlabeled_data["gzip_label"] = labels.reshape(-1)
    unlabeled_data.to_csv("gc_news_gzip_labels.csv", index=False)

    print("Full Set Classified!")

def main() -> None:
    print("\nFetching data...")
    labeled_data = pd.read_csv(DATA_DIR + "/ground_truth/labeled_data.csv", header=None, names=["text","label"])
    unbalanced_training_data = pd.read_csv(DATA_DIR + "/ground_truth/labeled_data_unbalanced.csv", header=None, names=["text","label"])
    unlabeled_data = pd.read_csv(DATA_DIR + "/gcnews/gc_news_unlabeled.csv")

    all_texts = np.array(labeled_data.text)
    all_labels = np.array(labeled_data.label)

    train_dev_texts, test_texts, train_dev_labels, test_labels = train_test_split(all_texts, all_labels, train_size=100, stratify=all_labels)

    skf = StratifiedKFold(n_splits=5)

    sleep(1)

    k_scores = []
    for top_k in range(1,11):
        scores = []
        for i, (train_index, dev_index) in enumerate(skf.split(train_dev_texts, train_dev_labels)):
            train_text, dev_text = train_dev_texts[train_index], train_dev_texts[dev_index]
            train_labels, dev_labels = train_dev_labels[train_index], train_dev_labels[dev_index]

            model = fit_model(train_text, train_labels)
            (distances, labels, similar_samples) = model.predict(
                x=dev_text,
                top_k=top_k
            )

            scores.append(accuracy_score(dev_labels, labels.reshape(-1)))
        k_scores.append(np.mean(scores))
        print("w/ topk = %d - accuracy: %d percent" % (top_k, np.mean(scores)*100))

    top_k = np.argmax(k_scores) + 1
    model = fit_model(train_dev_texts, train_dev_labels)
    (distances, labels, similar_samples) = model.predict(
        x=test_texts,
        top_k=top_k
    )

    output = []
    top_labels = []
    top_preds = []
    for i, (train_index, dev_index) in enumerate(skf.split(all_texts, all_labels)):
        train_text, dev_text = all_texts[train_index], all_texts[dev_index]
        train_labels, dev_labels = all_labels[train_index], all_labels[dev_index]

        model = fit_model(train_text, train_labels)
        (distances, labels, similar_samples) = model.predict(
            x=dev_text,
            top_k=top_k
        )

        top_labels.append(dev_labels)
        top_preds.append(labels.reshape(-1))
        output.append(classification_report(dev_labels, labels.reshape(-1), output_dict=True))

    top_preds = np.concatenate(top_preds).ravel().tolist()
    top_labels = np.concatenate(top_labels).ravel().tolist()


    print("top_k = ", top_k)


    # Validation
    random_states = [100, 200, 300, 400, 500]
    output = []

    for rs in random_states:
        train_texts, test_texts, train_labels, test_labels = train_test_split(all_texts, all_labels, train_size=100, stratify=all_labels,
                                                                              random_state=rs)
        model = fit_model(train_texts, train_labels)
        (distances, labels, similar_samples) = model.predict(
            x=test_texts,
            top_k=top_k
        )

        output.append(classification_report(test_labels, labels.reshape(-1), output_dict=True))
        print("Random state %d: Achieved f1 = %.2f w/ top_k = %d in training." % (rs,
                                                                                  classification_report(test_labels,
                                                                                                        labels.reshape(-1),
                                                                                                        output_dict=True
                                                                                                        )['weighted avg']['f1-score'],
                                                                                  top_k)
              )

    scores = pd.json_normalize(output)

    accuracy = (np.mean(scores.accuracy), np.std(scores.accuracy))
    precision = np.mean(scores['weighted avg.precision']), np.std(scores['weighted avg.precision'])
    recall = np.mean(scores['weighted avg.recall']), np.std(scores['weighted avg.recall'])
    f1 = np.mean(scores['weighted avg.f1-score']), np.std(scores['weighted avg.f1-score'])

    print(
        "accuracy: \t %.4f +- %.4f \n"
        "precision: \t %.4f +- %.4f \n"
        "recall: \t %.4f +- %.4f \n"
        "f1-score: \t %.4f +- %.4f \n" % (accuracy[0], accuracy[1],
                                          precision[0], precision[1],
                                          recall[0], recall[1],
                                          f1[0], f1[1],
                                          ))



    print(classification_report(top_labels, top_preds))

    ubal_texts = np.array(unbalanced_training_data.text)
    ubal_labels = np.array(unbalanced_training_data.label)
    final_model = fit_model(all_texts, all_labels)
    infer_predict(final_model, unlabeled_data, top_k)


if __name__ == "__main__":
    main()
